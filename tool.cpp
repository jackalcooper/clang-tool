//===---- tools/extra/ToolTemplate.cpp - Template for refactoring tool ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements an empty refactoring tool using the clang tooling.
//  The goal is to lower the "barrier to entry" for writing refactoring tools.
//
//  Usage:
//  tool-template <cmake-output-dir> <file1> <file2> ...
//
//  Where <cmake-output-dir> is a CMake build directory in which a file named
//  compile_commands.json exists (enable -DCMAKE_EXPORT_COMPILE_COMMANDS in
//  CMake to get this output).
//
//  <file1> ... specify the paths of files in the CMake source tree. This path
//  is looked up in the compile command database. If the path of a file is
//  absolute, it needs to point into CMake's source tree. If the path is
//  relative, the current working directory needs to be in the CMake source
//  tree and the file must be in a subdirectory of the current working
//  directory. "./" prefixes in the relative files will be automatically
//  removed, but the rest of a relative path must be a suffix of a path in
//  the compile command line database.
//
//  For example, to use tool-template on all files in a subtree of the
//  source tree, use:
//
//    /path/in/subtree $ find . -name '*.cpp'|
//        xargs tool-template /path/to/build
//
//===----------------------------------------------------------------------===//
#undef NDEBUG
#include "clang/AST/ASTTypeTraits.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Execution.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Refactoring/AtomicChange.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Signals.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

#define declSetFn(func_name)                                                   \
  std::string getFuncName_##func_name() { return #func_name; }                 \
  auto has##func_name##Expr =                                                  \
      has(cxxMemberCallExpr(has(memberExpr(member(hasName(#func_name)))))      \
              .bind(getFuncName_##func_name()));

declSetFn(SetTensorDescInferFn);
declSetFn(SetLogicalTensorDescInferFn);
declSetFn(SetPhysicalTensorDescInferFn);
declSetFn(SetGetSbpFn);
declSetFn(SetSbpSignatureInferFn);
declSetFn(SetInputArgModifyFn);
declSetFn(SetOutputArgModifyFn);
declSetFn(SetOutputBlobTimeShapeInferFn);
declSetFn(SetNdSbpInferFn);
declSetFn(SetCheckAttrFn);
declSetFn(SetDataTypeInferFn);
declSetFn(SetDeviceInferFn);

#define listSetFnNames                                                         \
  hasSetTensorDescInferFnExpr, hasSetLogicalTensorDescInferFnExpr,             \
      hasSetPhysicalTensorDescInferFnExpr, hasSetGetSbpFnExpr,                 \
      hasSetSbpSignatureInferFnExpr, hasSetInputArgModifyFnExpr,               \
      hasSetOutputArgModifyFnExpr, hasSetOutputBlobTimeShapeInferFnExpr,       \
      hasSetNdSbpInferFnExpr, hasSetCheckAttrFnExpr,                           \
      hasSetDataTypeInferFnExpr, hasSetDeviceInferFnExpr

namespace {
class ToolTemplateCallback : public MatchFinder::MatchCallback {
public:
  ToolTemplateCallback(ExecutionContext &Context) : Context(Context) {}

  void run(const MatchFinder::MatchResult &Result) override {
    auto *D = Result.Nodes.getNodeAs<VarDecl>("decl");
    auto *C = Result.Nodes.getNodeAs<CXXMemberCallExpr>(
        getFuncName_SetDataTypeInferFn());
    auto *G =
        Result.Nodes.getNodeAs<CXXMemberCallExpr>(getFuncName_SetCheckAttrFn());
    assert(D);
    llvm::errs() << "D\n";
    assert(C);
    llvm::errs() << "C\n";
    assert(G);
    llvm::errs() << "G\n";
    // Use AtomicChange to get a key.
    if (C->getBeginLoc().isValid()) {
      C->getBeginLoc().dump(*Result.SourceManager);
      // C->dump();
      // for (auto a : C->arguments()) {
      //   a->dump();
      // }
    }
    // if (D->getBeginLoc().isValid()) {
    //   D->getBeginLoc().dump(*Result.SourceManager);
    //   D->dump();
    //   AtomicChange Change(*Result.SourceManager, D->getBeginLoc());
    //   Context.reportResult(Change.getKey(), D->getQualifiedNameAsString());
    // }
  }

  void onStartOfTranslationUnit() override {
    Context.reportResult("START", "Start of TU.");
  }
  void onEndOfTranslationUnit() override {
    Context.reportResult("END", "End of TU.");
  }

private:
  ExecutionContext &Context;
};
} // end anonymous namespace

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::OptionCategory ToolTemplateCategory("tool-template options");

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  auto Executor = clang::tooling::createExecutorFromCommandLineArgs(
      argc, argv, ToolTemplateCategory);

  if (!Executor) {
    llvm::errs() << "Executor create error: "
                 << llvm::toString(Executor.takeError()) << "\n";
    return 1;
  }

  ast_matchers::MatchFinder Finder;
  ToolTemplateCallback Callback(*Executor->get()->getExecutionContext());

  Finder.addMatcher(
      traverse(
          clang::ast_type_traits::TraversalKind::TK_IgnoreUnlessSpelledInSource,
          varDecl(hasGlobalStorage(),
                  hasType(cxxRecordDecl(matchesName("UserOpRegisterTrigger"))),
                  anyOf(listSetFnNames), anyOf(listSetFnNames),
                  anyOf(listSetFnNames), anyOf(listSetFnNames))
              .bind("decl")),
      &Callback);

  auto Err = Executor->get()->execute(newFrontendActionFactory(&Finder));
  if (Err) {
    llvm::errs() << "Executor execute error: " << llvm::toString(std::move(Err))
                 << "\n";
  }
  Executor->get()->getToolResults()->forEachResult(
      [](llvm::StringRef key, llvm::StringRef value) {
        // llvm::errs() << "----" << key.str() << "\n" << value.str() << "\n";
      });
}
