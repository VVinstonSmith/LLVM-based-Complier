#  基于LLVM的简单编译器

## 简介
一款基于LLVM的编译器，用于实现一种简单语言的解释性运行。
自主实现文本提取和AST构建，调用LLVM的相关接口实现IR代码生成和JIT编译。

## 特性支持
编译器虽然简单，但具有很多特性，**包括：**
	双精度浮点类型的数据；
	变量定义和函数定义；
	顶层表达式处理；
	if语句 和 for语句；
	单目和双目运算符的重载；
	JIT编译；
	

## 功能组成

### 文本提取
提取 关键词，变量，数字，注释。

### AST构建
解析从文本提取的内容，构建AST表达式，**包括：**
	数字和变量；
	括号表达式；
	函数调用；
	if-then 语句；
	for 语句；
	变量定义；
	单目运算符重载；
	双目运算符重载；
	复杂的符号运算表达式（优先级分析）；
	函数声明；
	函数定义；
	顶层表达式；

### IR代码生成
根据构建出的AST，生成IR代码。

**基础元素包括：**
	常量 (`ConstantFP`);
	函数 (`Function::Create`);
	已注册的函数 (`Module->getFunction`);
	基本块 (`BasicBlock::Create`);
	函数入口块 (`CreateEntryBlockAlloca`);

**调用 IRBuilder 生成的元素包括： **
	内存变量加载 (`IRBuilder->CreateLoad`);
	变量写回到内存 (`IRBuilder->CreateStore`);
	函数调用 (`IRBuilder->CreateCall`);
	加, 减, 乘, 运算 (`IRBuilder->Create[FAdd | FSub | FMul]`);
	比较运算 (`IRBuilder->Create[FCmpULT | FCmpONE]`);
	类型转换 (`IRBuilder->CreateUIToFP`);
	跳转与条件跳转 (`IRBuilder->Create[CondBr | Br]`);
	PHI结点 (`IRBuilder->CreatePHI`);
	函数返回语句 (`IRBuilder->CreateRet`);

定义优化遍(Pass)，对构建好的 IR 代码进行多重优化 (`InstCombinePass`, `ReassociatePass`, `GVNPass`, `SimplifyCFGPass`)

### JIT编译
将构建好的 IR 模块添加到 JIT编译器，
编译 (`TheJIT->addModule`),
寻找匿名函数的地址 (`TheJIT->lookup("__anon_expr").getAddress()`),
执行.



