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
using clang::ast_type_traits::TraversalKind::TK_IgnoreUnlessSpelledInSource;

enum SetFnType {
  SetTensorDescInferFn = 1,
  SetLogicalTensorDescInferFn = 2,
  SetPhysicalTensorDescInferFn = 3,
  SetGetSbpFn = 4,
  SetSbpSignatureInferFn = 5,
  SetInputArgModifyFn = 6,
  SetOutputArgModifyFn = 7,
  SetOutputBlobTimeShapeInferFn = 8,
  SetNdSbpInferFn = 9,
  SetCheckAttrFn = 10,
  SetDataTypeInferFn = 11,
  SetDeviceInferFn = 12
};

template <SetFnType> std::string getStaticFuncReturnType();
template <SetFnType> std::string getFuncName();
using clang::ast_matchers::internal::Matcher;
template <SetFnType> Matcher<clang::Stmt> getExpr();
template <SetFnType> std::string getStaticFuncDeclare();
auto hasLambdaExpr =
    has(cxxBindTemporaryExpr(hasDescendant(lambdaExpr().bind("lambda"))));
#define declSetFn(func_name, return_t, declare)                                \
  template <> std::string getFuncName<func_name>() { return #func_name; }      \
  template <> Matcher<clang::Stmt> getExpr<func_name>() {                      \
    return cxxMemberCallExpr(has(memberExpr(member(hasName(#func_name)))),     \
                             hasLambdaExpr)                                    \
        .bind(#func_name);                                                     \
  }                                                                            \
  template <> std::string getStaticFuncReturnType<func_name>() {               \
    return #return_t;                                                          \
  }                                                                            \
  template <> std::string getStaticFuncDeclare<func_name>() { return #declare; }

declSetFn(SetTensorDescInferFn, "UNDEFINED", "UNDEFINED");
declSetFn(SetLogicalTensorDescInferFn, Maybe<void>,
          InferLogicalTensorDesc(user_op::InferContext *ctx));
declSetFn(SetPhysicalTensorDescInferFn, Maybe<void>,
          InferPhysicalTensorDesc(user_op::InferContext *ctx));
declSetFn(SetGetSbpFn, Maybe<void>, GetSbp(user_op::SbpContext *ctx));
declSetFn(SetSbpSignatureInferFn, Maybe<void>,
          InferSbpSignature(user_op::InferSbpSignatureFnContext *ctx));
declSetFn(SetInputArgModifyFn, Maybe<void>,
          ModifyInputArg(GetInputArgModifier,
                         const user_op::UserOpConfWrapper &));
declSetFn(SetOutputArgModifyFn, "Maybe<void>",
          ModifyOutputArg(GetOutputArgModifier,
                          const user_op::UserOpConfWrapper &));
declSetFn(
    SetOutputBlobTimeShapeInferFn, Maybe<void>,
    InferOutputBlobTimeShape(user_op::InferOutputBlobTimeShapeFnContext *ctx));
declSetFn(SetNdSbpInferFn, Maybe<void>,
          InferNdSbp(user_op::InferNdSbpFnContext *ctx));
declSetFn(SetCheckAttrFn, Maybe<void>,
          CheckAttr(const user_op::UserOpDefWrapper &,
                    const user_op::UserOpConfWrapper &));
declSetFn(SetDataTypeInferFn, Maybe<void>,
          InferDataType(user_op::InferContext *ctx));
declSetFn(SetDeviceInferFn, Maybe < Symbol<Device>,
          InferDevice(user_op::DeviceInferContext *ctx));

namespace {
class ToolTemplateCallback : public MatchFinder::MatchCallback {
public:
  ToolTemplateCallback(ExecutionContext &Context) : Context(Context) {}
  void checkAndDumpVarDecl(const MatchFinder::MatchResult &Result,
                           std::string name) {
    auto *C = Result.Nodes.getNodeAs<VarDecl>(name);
    if (C && C->getBeginLoc().isValid()) {
      llvm::errs() << "[dump] " << name << "\n";
      AtomicChange Change(*Result.SourceManager, C->getBeginLoc());
      auto Err = Change.replace(
          *Result.SourceManager,
          CharSourceRange::getCharRange(C->getSourceRange()), "");
      if (Err) {
        llvm::errs() << "Change error: " << llvm::toString(std::move(Err))
                     << "\n";
      }
      Context.reportResult(Change.getKey(), C->getQualifiedNameAsString());
    } else {
      // llvm::errs() << "[absent] " << name << "\n";
    }
  }
  void checkAndDumpCXXMemberCallExpr(const MatchFinder::MatchResult &Result,
                                     std::string name) {
    auto *C = Result.Nodes.getNodeAs<CXXMemberCallExpr>(name);
    if (C && C->getBeginLoc().isValid()) {
      llvm::errs() << "[dump] " << name << "\n";
      assert(C->getNumArgs() == 1);
      auto arg0 = C->getArg(0);
      // C->dump();
    } else {
      // llvm::errs() << "[absent] " << name << "\n";
    }
  }

  void run(const MatchFinder::MatchResult &Result) override {
    checkAndDumpVarDecl(Result, "decl");
    llvm::Optional<std::string> staticFuncDeclare;
    llvm::Optional<std::string> staticFuncReturnType;
    const clang::CXXMemberCallExpr *Found = nullptr;
    const clang::CXXMemberCallExpr *C = nullptr;
#define tryConvert(func_name)                                                  \
  C = Result.Nodes.getNodeAs<CXXMemberCallExpr>(getFuncName<func_name>());     \
  if (C && C->getBeginLoc().isValid()) {                                       \
    assert(!Found);                                                            \
    llvm::errs() << "[found] " << getFuncName<func_name>() << "\n";            \
    assert(C->getNumArgs() == 1);                                              \
    staticFuncDeclare = getStaticFuncDeclare<func_name>();                     \
    staticFuncReturnType = getStaticFuncReturnType<func_name>();               \
    Found = C;                                                                 \
  }

    tryConvert(SetTensorDescInferFn);
    tryConvert(SetLogicalTensorDescInferFn);
    tryConvert(SetPhysicalTensorDescInferFn);
    tryConvert(SetGetSbpFn);
    tryConvert(SetSbpSignatureInferFn);
    tryConvert(SetInputArgModifyFn);
    tryConvert(SetOutputArgModifyFn);
    tryConvert(SetOutputBlobTimeShapeInferFn);
    tryConvert(SetNdSbpInferFn);
    tryConvert(SetCheckAttrFn);
    tryConvert(SetDataTypeInferFn);
    tryConvert(SetDeviceInferFn);
    auto *lambda = Result.Nodes.getNodeAs<LambdaExpr>("lambda");
    // lambda->dump();
    if (lambda) {
      auto body = lambda->getBody();
      clang::SourceManager *sm = Result.SourceManager;
      clang::SourceLocation b(body->getBeginLoc());
      clang::SourceLocation e(body->getEndLoc());
      auto body_str =
          staticFuncReturnType.getValue() + " " + staticFuncDeclare.getValue() +
          std::string(sm->getCharacterData(b),
                      sm->getCharacterData(e) - sm->getCharacterData(b) + 1);
      llvm::outs() << body_str << "\n";
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
          TK_IgnoreUnlessSpelledInSource,
          varDecl(
              hasGlobalStorage(),
              hasType(cxxRecordDecl(matchesName("UserOpRegisterTrigger"))),
              eachOf(hasDescendant(getExpr<SetTensorDescInferFn>()),
                     hasDescendant(getExpr<SetLogicalTensorDescInferFn>()),
                     hasDescendant(getExpr<SetPhysicalTensorDescInferFn>()),
                     hasDescendant(getExpr<SetGetSbpFn>()),
                     hasDescendant(getExpr<SetSbpSignatureInferFn>()),
                     hasDescendant(getExpr<SetInputArgModifyFn>()),
                     hasDescendant(getExpr<SetOutputArgModifyFn>()),
                     hasDescendant(getExpr<SetOutputBlobTimeShapeInferFn>()),
                     hasDescendant(getExpr<SetNdSbpInferFn>()),
                     hasDescendant(getExpr<SetCheckAttrFn>()),
                     hasDescendant(getExpr<SetDataTypeInferFn>()),
                     hasDescendant(getExpr<SetDeviceInferFn>())))
              .bind("decl")),
      &Callback);

  auto Err = Executor->get()->execute(newFrontendActionFactory(&Finder));
  if (Err) {
    llvm::errs() << "Executor execute error: " << llvm::toString(std::move(Err))
                 << "\n";
  }
  Executor->get()->getToolResults()->forEachResult(
      [](llvm::StringRef key, llvm::StringRef value) {
        llvm::errs() << "----" << key.str() << "\n" << value.str() << "\n";
      });
}
