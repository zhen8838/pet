# Install Pet Python Interface On Mac M1

⚠️ 由于我已经将对应m1需要的修改进行了push, linux系统下如果不使用clang可能需要回退关于rpath的改动.
```sh
cd pet/
# use brew install automake
export CFLAGS="-I/Users/lisa/miniforge3/envs/ci/include -g" # please use conda install gmp NOTE need replace by your path
export CXXFLAGS="-g"
export LDFLAGS="-L/Users/lisa/miniforge3/envs/ci/lib -Wl,-rpath,/Users/lisa/miniforge3/envs/ci/lib" # gmp.dylib
./configure --prefix=`pwd`/build --with-clang-prefix=/Users/lisa/Documents/llvm-project-llvmorg-17.0.4/out/install/release # the custom llvm install path
export CPATH="$(xcrun --show-sdk-path)/usr/include"
make
# note 编译会报错isl_printer_to_str函数有error, 这是因为原来isl printer接口名起的不合理, 接口导出工具会认为他是一个返回string的函数.
# 因为修改起来太麻烦, 这里先忽略问题, 出错后我们编译python所依赖的部分也是没问题的:
make isl.py
make pet
make libpet.la

export DYLD_LIBRARY_PATH="`pwd`/.libs:`pwd`/isl/.libs:/Users/lisa/Documents/llvm-project-llvmorg-17.0.4/out/install/release/lib"
export PYTHONPATH="`pwd`/interface:`pwd`:$PYTHONPATH"
```

注意, 如果只需要使用isl部分(比如pet会与mlir产生冲突)时, 可以只编译并使用isl:
```sh
cd pet/isl
make interface/isl.py
export DYLD_LIBRARY_PATH="`pwd`/.libs:/Users/lisa/Documents/llvm-project-llvmorg-17.0.4/out/install/release/lib"
export PYTHONPATH="`pwd`/interface:$PYTHONPATH"
```

Requirements:

- pkg-config (http://www.freedesktop.org/wiki/Software/pkg-config)
	(not needed when compiling a release using the included isl)
- gmp (http://gmplib.org/)
- libyaml (http://pyyaml.org/wiki/LibYAML)
	(only needed if you want to compile the pet executable)
- LLVM/clang libraries, 3.5 or higher (http://clang.llvm.org/get_started.html)
	Unless you have some other reasons for wanting to use the git version,
	it's best to install the latest release (16.0).
	The git version occasionally introduces incompatibilities.
	Nevertheless, if you encounter any such incompatibilities, please
	report them so that they can be fixed.
	However, development versions from before the latest release
	are not supported.

	Also, if you are following the instructions on how to build
	from source, make sure you also install LLVM
	(through "make install").  You may want to specify an installation
	directory using the CMAKE_INSTALL_PREFIX cmake option.
	You may also want to set the LLVM_BUILD_LLVM_DYLIB option
	to enable the creation of a single shared library.

	If you configure older versions of LLVM using cmake, you may end up
	with clang libraries that have been compiled with -fno-rtti without
	this option appearing in the output of "llvm-config --cxxflags".
	You may then run into errors about undefined reference to the
	'typeinfo' of some classes.
	You will then have to add this option manually to CXXFLAGS while
	configuring pet.

	If you want to use the ubuntu package libclang-dev, then you need
	version 3.2 (ubuntu raring) or later.
	Older versions of this package did not include the required libraries.
	You may also need to install the llvm package for llvm-config.

Preparing:

Grab the latest release and extract it or get the source from
the git repository as follows.  This process requires autoconf,
automake, libtool and pkg-config.

	git clone git://repo.or.cz/pet.git
	cd pet
	./get_submodules.sh
	./autogen.sh

Compilation:

	./configure
	make
	make check

Use:

The main entry points are pet_scop_extract_from_C_source and
pet_transform_C_source.
The first function extracts a scop from the C source file with the given name
and returns it as a pet_scop.  The scop corresponds to the piece
of code delimited by

    #pragma scop

and

    #pragma endscop

The code in between needs to consist only of expression statements,
if statements and for statements.  All access relations and loop initializations
need to be piecewise quasi-affine.  Conditions are allowed to be non-affine,
in which case a separate statement is constructed to evaluate the condition.

The second function (pet_transform_C_source) iterates over all scops.

If the autodetect option has been set, pet will try to automatically
detect a scop and no pragmas are required.  On the other hand, pet
will not produce any warnings in this case as any code that does not
satisfy the requirements is considered to lie outside of the scop.

The layout of pet_scop is documented in include/pet.h.

An example application is given by pet_loopback.c,
which prints out code from the pet_scop without any transformation.


New releases are announced on http://groups.google.com/group/isl-announce

If you use pet, you can let me know by stacking
https://www.openhub.net/p/libpet on ohloh.

For bug reports, feature requests and questions,
contact isl-development@googlegroups.com

If you use pet for your research, you are invited to cite
the following paper.

@InProceedings{Verdoolaege2012pet,
    author = {Sven Verdoolaege and Tobias Grosser},
    title = {Polyhedral Extraction Tool},
    booktitle = {Second Int. Workshop on Polyhedral Compilation Techniques
		(IMPACT'12)},
    address = {Paris, France},
    month = jan,
    year = {2012}
}
