
 Copyright (c) 2013, Olumuyiwa Oluwasanmi
 All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

 3. Neither the name of the Olumuyiwa Oluwasanmi nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#Simple Parallel STL implementation.
* This is a header only multi-threaded implementation of some of the STL library in C++ using simple threading primitives.
	Currently work in progress..
* For all functions implemented should be called with the parallel namespace instead of the normal std namespaces for STL functions.
	For example parallel::for_each rather than std::for_each. The arguments should match.

##Dependencies
* The ideas used are generic enough that the header file should work for all c++11 compilers that completely implement the STL and then std::thread libraries for
c++11.Without needing to link with any other library. So far all the conditions for using the std::thread libraries are met for the individual compiler.

* The test file ParLibTest.cpp uses the boost testing framework and boost test library. 
* Doxyfile contains doxygen configuration so a documentation can be generated.

* Things left to do:
	1. Implement the STL functions based on sorting
	2. Improve the Code Documentation.
	3. Update the README file.
	4. Include doxygen configuration file
	5. Include code examples in README.


