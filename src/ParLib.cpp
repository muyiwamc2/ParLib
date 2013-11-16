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
	std::vector<double> two(1000);
	std::iota(one.begin(),one.end(), 1);
	std::cout << "starting transform" << two.size() << std::endl;
	parallel::transform(one.begin(), one.end(), two.begin(),
			[](int k)->double {return 3.14159*static_cast<double>(k);});
	std::for_each(one.begin(), one.end(), [](double k) {std::cout<< k<<std::endl;});
	std::cout<< " The sum is:"<< parallel::accumulate(one.begin(),one.end(),0)<<std::endl;

	return 0;
}
