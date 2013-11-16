//============================================================================
// Name        : ParLib.cpp
// Author      : Olumuyiwa
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <algorithm>
#include <iostream>
#include "ParLib.h"




int main() {
	std::cout << "!!!Hello World!!!" << std::endl; // prints !!!Hello World!!!
	 std::vector<int> one(1000);
	   std::iota(one.begin(),one.end(),1);
	   std::vector<double> two(1000);
	   std::cout<<"starting transform"<<std::endl;
	   parallel::transform(one.begin(),one.end(),two.begin(),[](int k)->double{ return 5.0+ 3.141592 *static_cast<double>(k);});
	   parallel::for_each(two.begin(),two.end(),[](double k){ std::cout<< k<<std::endl;});


	return 0;
}
