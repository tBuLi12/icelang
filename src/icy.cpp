#include "./IREmmiter/compiler.h"
#include "./parser/parse.h"
#include <filesystem>
#include <fstream>
#include <range/v3/all.hpp>
#include <ranges>
#include "iceStd.h"

struct Package {
    std::optional<std::filesystem::path> path;
    std::string id;
    std::string pathStr;
};

Package packageIdFromPath(std::string_view pathStr) {
    std::filesystem::path path{pathStr};
    auto id = path.filename().stem().string();
    auto pathAsStr = path.string();
    return Package{std::move(path), std::move(id), std::move(pathAsStr)};
}

std::vector<std::string> splitBySlash(std::string_view str) {
    return ranges::views::split_when(str, [](char c) { return c == '/'; }) |
        ranges::views::transform([](auto rng) {
            return rng | ranges::to<std::string>();
        }) | ranges::to<std::vector<std::string>>();
}

std::string toLowercase(std::string_view str) {
    return str | ranges::views::transform([](char c) { return std::tolower(c); } ) | ranges::to<std::string>();
}

std::pair<std::string, std::filesystem::path> moduleIdFromPath(
    std::unordered_map<std::string, Package>& packages, std::string_view path,
    Module& current
) {
    auto curPath = current.path.parent_path();
    auto colon = std::ranges::find(path, ':');
    std::string_view tail{path};
    std::string_view pckgName = current.packageName();
    std::vector<std::string> idSegments = splitBySlash(current.moduleId);
    
    if (idSegments.size() > 1)
        idSegments.pop_back();

    if (colon != path.end()) {
        pckgName = {path.begin(), colon};
        if (!packages[std::string{pckgName}].path) return std::pair<std::string, std::filesystem::path>{pckgName, std::filesystem::path{}};

        curPath = packages[std::string{pckgName}].path->parent_path();
        idSegments = {std::string{pckgName}};
        tail = {++colon, path.end()};
    }

    auto segments = splitBySlash(tail);

    if (segments.size() > 0 && !(segments.front() == "." || segments.front() == "..")) {
        curPath = packages[std::string{pckgName}].path->parent_path();
        idSegments.resize(1);
    }

    auto processSegment = [&](std::string& seg) {
        if (seg == ".") {
            return;
        }
        if (seg == "..") {
            if (idSegments.size() < 2) {
                std::cout << "invalid path1" << std::endl;
                throw "";
            }
            curPath = curPath.parent_path();
            idSegments.pop_back();
        } else if (ranges::all_of(seg, [](char c) {
                       return std::isalnum(c) || c == '_';
                   })) {
            curPath /= seg;
            idSegments.push_back(toLowercase(seg));
        } else {
            std::cout << "invalid path2 " << seg << std::endl;
            throw "";
        }
    };

    for (auto seg : segments) {
        processSegment(seg);
    }

    if (idSegments.size() == 2 && idSegments[0] == idSegments[1])
        idSegments.pop_back();

    curPath.replace_extension(".ice");

    return {
        fmt::format("{}", fmt::join(idSegments, "/")),
        curPath,
    };
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cout << "pass path" << std::endl;
        throw "";
    }

    std::unordered_map<std::string, Package> packages{};
    std::vector<std::ifstream*> files{};
    std::vector<Module*> modules{};

    packages["std"] = Package{{}, "std", "std"};

    std::optional<std::string> fid;
    for (size_t i = 1; i < argc; ++i) {
        auto package = packageIdFromPath(argv[i]);
        if (!fid) {
            fid = package.id;
        }
        if (packages.find(package.id) != packages.end()) {
            std::cout <<  "duplicate package" << std::endl;
        } else {
            packages[package.id] = std::move(package);
        }
    }

    std::stringstream stdLib{stdIce};
    Source stdsource{stdLib, "std"};
    auto [program, log] = parseProgram(stdsource);
    if (!log.errorsAreEmpty()) {
        log.printDiagnosticsTo(std::cerr);
    }
    modules.emplace_back(new Module{
            stdsource, std::move(program), "std"});

    for (auto& [_, package] : packages) {
        if (!package.path) continue;
        files.push_back(new std::ifstream{});
        files.back()->open(*package.path, std::ios::binary);
        if (!*files.back()) {
            std::cout << *package.path << " not found2" << std::endl;
            throw "";
        }
        Source source{*files.back(), package.pathStr};
        auto [program, log] = parseProgram(source);
        if (!log.errorsAreEmpty()) {
            log.printDiagnosticsTo(std::cerr);
        }
        modules.emplace_back(new Module{
            source, std::move(program), package.id});
    }

    for (size_t idx = 1; idx < modules.size(); ++idx) {
        for (auto& import : modules[idx]->program.imports) {
            auto [moduleId, path] = moduleIdFromPath(
                packages, import.path.value, *modules[idx]
            );
            auto maybeMod = std::ranges::find(modules, moduleId, &Module::moduleId);
            if (maybeMod !=
                modules.end()) {
                modules[idx]->imports[import.name.value] = {
                    std::distance(modules.begin(), maybeMod), &import.visibility};
                continue;
            }

            files.push_back(new std::ifstream{});
            files.back()->open(path, std::ios::binary);
            if (!*files.back()) {
                std::cout << path.string() << " not found1" << std::endl;
                throw "";
            }
            Source source{*files.back(), import.path.value};
            auto [program, log] = parseProgram(source);
            if (!log.errorsAreEmpty()) {
                log.printDiagnosticsTo(std::cerr);
            }

            modules.push_back(new Module{
                source, std::move(program), moduleId, std::move(path)});
        }
    }


    auto result = compile(modules);
    for (auto f : files) {
        f->close();
    }

    if (!result) {
        return 1;
    }

    auto [llvmContext, llvmModule, targetMachine] = std::move(*result);

    llvmModule->print(llvm::outs(), nullptr);

    llvm::legacy::PassManager passManager{};

    std::error_code errCode;
    llvm::raw_fd_ostream outputFile{"iout.o", errCode, llvm::sys::fs::OF_None};
    if (errCode) {
        std::cerr << "could not open out file: " << errCode << std::endl;
        return 1;
    }
    if (targetMachine->addPassesToEmitFile(
            passManager, outputFile, nullptr, llvm::CGFT_ObjectFile
        )) {
        std::cerr << "can't emit" << std::endl;
        return 1;
    }

    passManager.run(*llvmModule);
    outputFile.flush();
        std::cerr << "done" << std::endl;
    llvmModule.reset();
    return 0;
}
