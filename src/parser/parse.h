#ifndef ICY_PARSE_H
#define ICY_PARSE_H

#include "./ast.h"

std::pair<ast::AST, logs::MessageLog> parseProgram(Source source);

#endif
