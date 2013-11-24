//============================================================================
// Name        : ParLib.cpp
// Author      : Olumuyiwa
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE ParLibTest
#include <algorithm>
#include <iostream>
#include <functional>
#include "ParLib.h"
//#include <boost/test/minimal.hpp>
#include <boost/test/included/unit_test.hpp>
//#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(Transform)
BOOST_AUTO_TEST_CASE( transform_test ){


	std::vector<int> one(1000);
	std::vector<double> two(1000);
	std::iota(one.begin(),one.end(), 1);
	parallel::transform(one.begin(), one.end(), two.begin(),
			[&](int k)->double {return 259*static_cast<double>(k);});
	BOOST_CHECK(two[20]==21.0*259.0);
}
BOOST_AUTO_TEST_CASE( transform_test2 ){
	using namespace std;
	vector<int> one(1000);
	vector<int> two(1000);
	vector<int>three(1000);
	iota(one.begin(),one.end(), 1);
	iota(two.begin(),two.end(),0);

	function<int(int,int)> binop =[&](int k, int x)->int{ return 259*k* x;};
	parallel::transform(one.begin(), one.end(), two.begin(),three.begin(),[&](int k, int x)->int{ return 259*k* x;}
			);
	BOOST_CHECK(three[20]==20*21*259);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(ForEach)
BOOST_AUTO_TEST_CASE( foreach_test1){
	using namespace std;
	vector<int> one(1000);
	iota(one.begin(),one.end(), 1);
	//function<void(int&)> op = [&](int &k)->void{ one[k-1]= k*30; };
	parallel::for_each(one.begin(), one.end(),[&](int &k)->void{ one[k-1]= k*30; });
	BOOST_CHECK(one[20]==21*30);

}
BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(Accumulate)
BOOST_AUTO_TEST_CASE( accummulate_test1){
	using namespace std;
	int val;
	vector<int> one(1000);
	iota(one.begin(),one.end(), 1);
	//function<void(int&)> op = [&](int &k)->void{ one[k-1]= k*30; };

	val= parallel::accumulate(one.begin(),one.end(),0);
	BOOST_CHECK(val==500500);

}

BOOST_AUTO_TEST_CASE( accummulate_test2){
	using namespace std;
	int val;
	vector<int> one(1000);
	iota(one.begin(),one.end(), 1);
	//function<void(int&)> op = [&](int &k)->void{ one[k-1]= k*30; };

	val= parallel::accumulate(one.begin(),one.end(),0,[](int k, int j)->int{return k+ j;});
	BOOST_CHECK(val==500500);


	std::cout<< " The sum is:"<< parallel::accumulate_if(one.begin(),one.end(),0,[](int k)->bool{return (k%10)==0 ;})<<std::endl;
	std::cout<< "find number 177:"<<*(parallel::find(one.begin(),one.end(), val))<<std::endl;
	std::cout<< "what is the count:"<< parallel::count(one.begin(),one.end(),val)<<std::endl;
	std::cout<< "the count if test:"<< parallel::count_if(one.begin(),one.end(),[](int k)->bool{return (k%10)==0;})<<std::endl;

}
BOOST_AUTO_TEST_SUITE_END()
/*init_unit_test_suite(int argc, char**argv){
	return 0;
}*/
//int unit_test_main( init_unit_test_func init_func, int argc, char* argv[] ){ return 0;}
