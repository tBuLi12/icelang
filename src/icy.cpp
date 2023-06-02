#include "./IREmmiter/IREmmiter.cpp"
#include "./parser/parse.h"
#include <filesystem>
#include <fstream>
#include <range/v3/all.hpp>
#include <ranges>

struct Package {
    std::filesystem::path path;
    std::string id;
    std::string pathStr;
};

Package packageIdFromPath(std::string_view pathStr) {
    std::filesystem::path path{pathStr};
    auto id = path.filename().stem().string();
    return Package{std::move(path), std::move(id), path.string()};
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
        curPath = packages[std::string{pckgName}].path.parent_path();
        idSegments = {std::string{pckgName}};
        tail = {++colon, path.end()};
    }

    std::cout << "getting segmentrs" << std::endl;
    auto segments = splitBySlash(tail);
    std::cout << "got segments " << segments.size() << std::endl;

    if (segments.size() > 0 && !(segments.front() == "." || segments.front() == "..")) {
        curPath = packages[std::string{pckgName}].path.parent_path();
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

    auto ctx = std::make_unique<llvm::LLVMContext>();
    auto mod = std::make_unique<llvm::Module>("my cool jit", *ctx);
    auto builder = std::make_unique<llvm::IRBuilder<>>(*ctx);

    auto intt = llvm::Type::getInt32Ty(*ctx);
    auto nullt = llvm::StructType::create(*ctx, {}, "null");
    auto boolt = llvm::Type::getInt1Ty(*ctx);
    auto ptr = llvm::PointerType::get(*ctx, 0);

    std::unordered_map<std::string, type::BuiltIn*> builtInTypes{
        {"int", new type::BuiltIn{intt, "int"}},
        {"null", new type::BuiltIn{nullt, "null"}},
        {"bool", new type::BuiltIn{boolt, "bool"}},
        {"ptr", new type::BuiltIn{ptr, "ptr"}},
    };

    std::unordered_map<std::string, Package> packages{};
    std::vector<std::ifstream*> files{};
    std::vector<Module*> modules{};

    std::optional<std::string> fid;
    for (size_t i = 1; i < argc; ++i) {
        auto package = packageIdFromPath(argv[i]);
        if (!fid) {
            fid = package.id;
        }
        packages[package.id] = std::move(package);
    }

    for (auto& [_, package] : packages) {
        files.push_back(new std::ifstream{});
        files.back()->open(package.path, std::ios::binary);
        if (!*files.back()) {
            std::cout << package.path << " not found2" << std::endl;
            throw "";
        }
        Source source{*files.back(), package.pathStr};
        auto [program, log] = parseProgram(source);
        if (!log.errorsAreEmpty()) {
            log.printDiagnosticsTo(std::cout);
        }
        modules.emplace_back(new Module{
            source, std::move(program), package.id});
    }

    for (size_t idx = 0; idx < modules.size(); ++idx) {
        std::cout << "processing module " << idx << std::endl;
        for (auto& import : modules[idx]->program.imports) {
            std::cout << "processing import " << import.path.value << std::endl;
            auto [moduleId, path] = moduleIdFromPath(
                packages, import.path.value, *modules[idx]
            );
            std::cout << "resolved" << std::endl;
            if (std::ranges::find(modules, moduleId, &Module::moduleId) !=
                modules.end()) {
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
                log.printDiagnosticsTo(std::cout);
            }

            modules[idx]->imports[import.name.value] = modules.size();
            modules.push_back(new Module{
                source, std::move(program), moduleId, std::move(path)});
        }
    }
    
    std::cout <<  "donFFe" << std::endl;

    for (auto& [_, pkg] : packages) {
        std::cout << pkg.id  << std::endl;
    }
        std::cout << std::endl;

    for (auto& mod : modules) {
        // mod.
        std::cout << mod->moduleId
                   << std::endl;
    }
    // return 0;
    TypeChecker tc{
        builtInTypes,
    };
    std::cout << "parsing done" << std::endl;

    tc.check(modules);

    tc.implementationScope.currentBounds.block = nullptr;
    tc.implementationScope.currentBounds.local = nullptr;
    IRVisitor ir{
        Source {*files.front(), packages[*fid].pathStr}, std::move(ctx), std::move(builder),
        std::move(mod), builtInTypes,   std::move(tc.implementationScope)};

    ir.vecDeclaration = tc.typeScope.at("std::Vector");
    fmt::println("here");
    ir.allocFunc = Function{tc.funcScope.at("std::rtAlloc"), {}};
    ir.sliceFunc = Function{tc.funcScope.at("std::rtSlice"), {}};
    ir.freeFunc = Function{tc.funcScope.at("std::rtFree"), {}};
    ir.moveFunc = Function{tc.funcScope.at("std::rtMove"), {}};
    fmt::println("aaaa");
    ir.copyDeclaration = tc.traitScope.at("std::Copy");
    ir.dropDeclaration = tc.traitScope.at("std::Drop");
    ir.modules = &modules;

    auto targetTriple = llvm::sys::getDefaultTargetTriple();

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string targetErr;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, targetErr);
    if (!target) {
        std::cerr << targetErr;
        return 1;
    }

    auto targetMachine =
        target->createTargetMachine(targetTriple, "generic", "", {}, {});

    ir.currentModule->setDataLayout(targetMachine->createDataLayout());
    ir.currentModule->setTargetTriple(targetTriple);

    auto mainBody =
        tc.funcScope.find(modules.front()->moduleId + "::main");
    if (mainBody == tc.funcScope.end()) {
        std::cout << "main undefined" << std::endl;
        throw "";
    }
    ir.emitMain(std::move(mainBody->second->body));

    for (auto f : files) {
        f->close();
    }

    ir.currentModule->print(llvm::outs(), nullptr);

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

    passManager.run(*ir.currentModule);
    outputFile.flush();

    return 0;
}
