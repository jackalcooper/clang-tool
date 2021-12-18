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
std::string convertToCamelFromSnakeCase(const std::string &input,
                                        bool capitalizeFirst) {
  if (input.empty())
    return "";

  std::string output;
  output.reserve(input.size());

  // Push the first character, capatilizing if necessary.
  if (capitalizeFirst && std::islower(input.front()))
    output.push_back(toupper(input.front()));
  else
    output.push_back(input.front());

  // Walk the input converting any `*_[a-z]` snake case into `*[A-Z]` camelCase.
  for (size_t pos = 1, e = input.size(); pos < e; ++pos) {
    if (input[pos] == '_' && pos != (e - 1) && std::islower(input[pos + 1]))
      output.push_back(toupper(input[++pos]));
    else
      output.push_back(input[pos]);
  }
  return output;
}
bool IsConvOp(const std::string &op_name) {
  return op_name.rfind("conv", 0) == 0 &&
         op_name.find("grad") == std::string::npos;
}
std::string GetConvOpClassName(const std::string &op_name) {
  std::string ret(convertToCamelFromSnakeCase(op_name, true));
  // NOTE: should change form conv => Convolution ?
  return ret;
}
#include <regex>
std::string PostProcessClassName(const std::string &op_name) {
  std::string ret = op_name;
  ret = std::regex_replace(ret, std::regex("pool"), "Pool");
  ret = std::regex_replace(ret, std::regex("_1d"), "1D");
  ret = std::regex_replace(ret, std::regex("_2d"), "2D");
  ret = std::regex_replace(ret, std::regex("_3d"), "3D");
  ret = std::regex_replace(ret, std::regex("1d"), "1D");
  ret = std::regex_replace(ret, std::regex("2d"), "2D");
  ret = std::regex_replace(ret, std::regex("3d"), "3D");
  return ret;
}

std::string GetOpClassName(const std::string &op_name) {
  std::string ret = "";
  if (IsConvOp(op_name)) {
    ret = GetConvOpClassName(op_name);
  } else {
    ret = convertToCamelFromSnakeCase(op_name, true);
  }
  return PostProcessClassName(ret);
}

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
auto hasLambdaExpr = optionally(
    has(cxxBindTemporaryExpr(hasDescendant(lambdaExpr().bind("lambda")))));
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

declSetFn(SetTensorDescInferFn, Maybe<void>,
          InferLogicalTensorDesc(user_op::InferContext *ctx));
declSetFn(SetLogicalTensorDescInferFn, Maybe<void>,
          InferLogicalTensorDesc(user_op::InferContext *ctx));
declSetFn(SetPhysicalTensorDescInferFn, Maybe<void>,
          InferPhysicalTensorDesc(user_op::InferContext *ctx));
declSetFn(SetGetSbpFn, Maybe<void>, GetSbp(user_op::SbpContext *ctx));
declSetFn(SetSbpSignatureInferFn, Maybe<void>,
          InferSbpSignature(user_op::InferSbpSignatureFnContext *ctx));
declSetFn(SetInputArgModifyFn, Maybe<void>,
          ModifyInputArg(GetInputArgModifier GetInputArgModifierFn,
                         const user_op::UserOpConfWrapper &conf));
declSetFn(SetOutputArgModifyFn, "Maybe<void>",
          ModifyOutputArg(GetOutputArgModifier GetOutputArgModifierFn,
                          const user_op::UserOpConfWrapper &conf));
declSetFn(
    SetOutputBlobTimeShapeInferFn, Maybe<void>,
    InferOutputBlobTimeShape(user_op::InferOutputBlobTimeShapeFnContext *ctx));
declSetFn(SetNdSbpInferFn, Maybe<void>,
          InferNdSbp(user_op::InferNdSbpFnContext *ctx));
declSetFn(SetCheckAttrFn, Maybe<void>,
          CheckAttr(const user_op::UserOpDefWrapper &def,
                    const user_op::UserOpConfWrapper &conf));
declSetFn(SetDataTypeInferFn, Maybe<void>,
          InferDataType(user_op::InferContext *ctx));
declSetFn(SetDeviceInferFn, Maybe<Symbol<Device>>,
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
    auto SetTensorDescInferFnExpr = Result.Nodes.getNodeAs<CXXMemberCallExpr>(
        getFuncName<SetTensorDescInferFn>());
    auto op_type_name =
        Result.Nodes.getNodeAs<clang::StringLiteral>("op_type_name");
    auto OpCamelName = GetOpClassName(op_type_name->getString()) + "Op";
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
    auto prefix = "/* static */ " + staticFuncReturnType.getValue() + " " +
                  OpCamelName + "::" + staticFuncDeclare.getValue() + " ";
    const std::string ADD_CODE_HERE = "{\nADD_CODE_HERE;\n}";
    clang::SourceManager *sm = Result.SourceManager;
    if (lambda) {
      auto body = lambda->getBody();
      clang::SourceLocation b(body->getBeginLoc());
      clang::SourceLocation e(body->getEndLoc());
      auto body_str =
          std::string(sm->getCharacterData(b),
                      sm->getCharacterData(e) - sm->getCharacterData(b) + 1);
      if (body_str.empty()) {
        body_str = ADD_CODE_HERE;
      }
      llvm::outs() << prefix << body_str << "\n\n";
    } else {
      auto arg = Found->getArg(0);
      clang::SourceLocation b(arg->getBeginLoc());
      clang::SourceLocation e(arg->getEndLoc());
      auto body_str =
          std::string(sm->getCharacterData(b),
                      sm->getCharacterData(e) - sm->getCharacterData(b) + 1);
      llvm::outs() << prefix << "{\n"
                   << "return " << body_str << "(ctx);"
                   << "\n}\n\n";
    }
    if (SetTensorDescInferFnExpr) {
      llvm::outs() << "/*static*/ Maybe<void> " << OpCamelName
                   << "::InferPhysicalTensorDesc(user_op::InferContext* "
                      "ctx) {return InferLogicalTensorDesc(ctx);}\n\n";
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
  llvm::outs() << "#include \"oneflow/core/framework/op_generated.h\"\n\n";
  llvm::outs() << "namespace oneflow {\n\n";
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

  auto CheckAndGetOpRegistryExpr = cxxMemberCallExpr(
      has(memberExpr(member(hasName("CheckAndGetOpRegistry")))),
      hasDescendant(stringLiteral().bind("op_type_name")));
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
                     hasDescendant(getExpr<SetDeviceInferFn>())),
              hasDescendant(CheckAndGetOpRegistryExpr))
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
