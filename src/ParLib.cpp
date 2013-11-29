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
#include <numeric>
#include <iostream>
#include <functional>
#include "ParLib.h"
//#include <boost/test/minimal.hpp>
#include <boost/test/included/unit_test.hpp>
//#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(Transform)
	BOOST_AUTO_TEST_CASE( transform_test ) {

		std::vector<int> one(1000);
		std::vector<double> two(1000);
		std::iota(one.begin(), one.end(), 1);
		parallel::transform(one.begin(), one.end(), two.begin(),
				[&](int k)->double {return 259*static_cast<double>(k);});
		BOOST_CHECK(two[20] == 21.0 * 259.0);
	}
	BOOST_AUTO_TEST_CASE( transform_test2 ) {
		using namespace std;
		vector<int> one(1000);
		vector<int> two(1000);
		vector<int> three(1000);
		iota(one.begin(), one.end(), 1);
		iota(two.begin(), two.end(), 0);

		function<int(int, int)> binop = [&](int k, int x)->int {return 259*k* x;};
		parallel::transform(one.begin(), one.end(), two.begin(), three.begin(),
				[&](int k, int x)->int {return 259*k* x;});
		BOOST_CHECK(three[20] == 20 * 21 * 259);
	}

	BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(ForEach)
	BOOST_AUTO_TEST_CASE( foreach_test1) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		//function<void(int&)> op = [&](int &k)->void{ one[k-1]= k*30; };
		parallel::for_each(one.begin(), one.end(), [&](int &k)->void {one[k-1]= k*30;});
		BOOST_CHECK(one[20] == 21 * 30);

	}
	BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(Accumulate)
	BOOST_AUTO_TEST_CASE( accumulate_test1) {
		using namespace std;
		int val;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		//function<void(int&)> op = [&](int &k)->void{ one[k-1]= k*30; };

		val = parallel::accumulate(one.begin(), one.end(), 0);
		BOOST_CHECK(val == 500500);
	}

	BOOST_AUTO_TEST_CASE( accumulate_test2) {
		using namespace std;
		int val;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		//function<void(int&)> op = [&](int &k)->void{ one[k-1]= k*30; };

		val = parallel::accumulate(one.begin(), one.end(), 0, [](int k, int j)->int {return k+ j;});
		BOOST_CHECK(val == 500500);
	}
	BOOST_AUTO_TEST_SUITE_END()BOOST_AUTO_TEST_SUITE
(Accumulate_If)
	BOOST_AUTO_TEST_CASE( accummulate_if_test1) {
		using namespace std;
		int val;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		//function<void(int&)> op = [&](int &k)->void{ one[k-1]= k*30; };

		val = parallel::accumulate_if(one.begin(), one.end(), 0,
				[](int k)->bool {return (k%10)==0;});
		BOOST_CHECK(val == 50500);
	}

	BOOST_AUTO_TEST_CASE( accummulate_if_test2) {
		using namespace std;
		int val;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		function<int(int, int)> op = [&](int i, int j)->int {return i+j;};

		val = parallel::accumulate_if(one.begin(), one.end(), 0,
				[](int k)->bool {return (k%10)==0;}, op);
		BOOST_CHECK(val == 50500);
	}
	BOOST_AUTO_TEST_SUITE_END()
//begin inner product test
BOOST_AUTO_TEST_SUITE(Inner_Product)
	BOOST_AUTO_TEST_CASE( inner_product_test1) {
		using namespace std;
		int val;
		vector<int> one(1000);
		vector<int> two(1000);
		iota(one.begin(), one.end(), 1);
		iota(two.begin(), two.end(), 1);
		int v = std::inner_product(one.begin(), one.end(), two.begin(), 0);

		val = parallel::inner_product(one.begin(), one.end(), two.begin(), 0);
		BOOST_CHECK(val == v);
	}

	BOOST_AUTO_TEST_CASE( inner_product_test2) {
		using namespace std;
		long int val;
		vector<long int> one(1000);
		vector<long int> two(1000);
		iota(one.begin(), one.end(), 1);
		iota(two.begin(), two.end(), 1);
		function<long int(long int, long int)> op1 =
				[&](long int i, long int j)->long int {return i+j;};
		function<long int(long int, long int)> op2 =
				[&](long int i, long int j)->long int {return i*j;};

		long int v = std::inner_product(one.begin(), one.end(), two.begin(), 0, op1, op2);

		val = parallel::inner_product(one.begin(), one.end(), two.begin(), 0, op1, op2);

		BOOST_CHECK(val == v);
	}
	BOOST_AUTO_TEST_SUITE_END()
	BOOST_AUTO_TEST_SUITE(Adjacent_Difference)
	BOOST_AUTO_TEST_CASE( adjacent_difference_test1) {
			using namespace std;
			vector<int> one(1000);
			vector<int> two(1000);
			vector<int> three(1000);
 			iota(one.begin(), one.end(), 1);
			int j;
			for(auto &i :one) {++j; i +=j;}
			std::adjacent_difference(one.begin(), one.end(), two.begin());

			parallel::adjacent_difference(one.begin(), one.end(), three.begin());
			//for(auto k: three)cout<<k<<std::endl;
			BOOST_CHECK(std::equal(two.begin(),two.end(),three.begin()));
		}
	BOOST_AUTO_TEST_CASE( adjacent_difference_test2) {
				using namespace std;
				vector<int> one(1000);
				vector<int> two(1000);
				vector<int> three(1000);
	 			iota(one.begin(), one.end(), 1);
				int j;
				//function<int(int,int)> op = [&](int &a, int &b)->int{ return b-a;} ;
				for(auto &i :one) {++j; i =i+j;}
				std::adjacent_difference(one.begin(), one.end(), two.begin(),[](int a, int b)->int{ return a-b;});

				parallel::adjacent_difference(one.begin(), one.end(),  three.begin(),[](int a, int b)->int{ return a-b;});
				//for(auto k: three)cout<<k<<std::endl;
				BOOST_CHECK(std::equal(two.begin(),two.end(),three.begin()));
			}
	BOOST_AUTO_TEST_SUITE_END()
BOOST_AUTO_TEST_SUITE(Find)
	BOOST_AUTO_TEST_CASE( find_test1) {
		using namespace std;
		int val = 177;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);

		auto val2 = parallel::find(one.begin(), one.end(), val);
		BOOST_CHECK(val == *val2);
	}
	BOOST_AUTO_TEST_CASE( find_if_test) {
		using namespace std;
		int val = 177;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		function<bool(int)> op = [](int i)->bool {return i==177;};
		auto val2 = parallel::find_if(one.begin(), one.end(), op);
		BOOST_CHECK(val == *val2);

	}
	BOOST_AUTO_TEST_CASE( adjacent_find_test1) {
		using namespace std;
		int val = 177;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		one[177] = 177;
		auto val2 = parallel::adjacent_find(one.begin(), one.end());
		BOOST_CHECK(val == *val2);
	}
	BOOST_AUTO_TEST_CASE( adjacent_find_test2) {
		using namespace std;
		int val = 177;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		one[177] = 177;
		function<bool(int, int)> op = [](int i, int j)->bool {return i==j;};
		auto val2 = parallel::adjacent_find(one.begin(), one.end(), op);
		BOOST_CHECK(val == *val2);

	}
	BOOST_AUTO_TEST_CASE( find_if_not_test) {
		using namespace std;
		int val = 177;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		function<bool(int&)> op = [](int& i)->bool {return i!=177;};
		auto val2 = parallel::find_if_not(one.begin(), one.end(), op);
		BOOST_CHECK(val == *val2);
	}
	BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(Count)
	BOOST_AUTO_TEST_CASE( count_test) {
		using namespace std;
		int val = 177;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		one[256] = 177;
		one[258] = 177;
		auto val2 = parallel::count(one.begin(), one.end(), val);

		BOOST_CHECK(val2 == 3);
	}

	BOOST_AUTO_TEST_CASE( count_if_test) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		auto val2 = parallel::count_if(one.begin(), one.end(), [](int k)->bool {return (k%10)==0;});
		BOOST_CHECK(val2 == 100);
	}
	BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(SimpleLogical)
	BOOST_AUTO_TEST_CASE(all_of_test) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 10);
		auto val = parallel::all_of(one.begin(), one.end(), [](int i)->bool {return ( i>9);});

		BOOST_CHECK(val);
	}

	BOOST_AUTO_TEST_CASE(any_of_test) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 10);
		auto val = parallel::any_of(one.begin(), one.end(), [](int &i)->bool {return ( i==177);});

		BOOST_CHECK(val);
	}

	BOOST_AUTO_TEST_CASE(none_of_test) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 10);
		auto val = parallel::none_of(one.begin(), one.end(), [](int &i)->bool {return ( i==2013);});

		BOOST_CHECK(val);
	}

	BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(Equal)
	BOOST_AUTO_TEST_CASE(equal_test1) {
		using namespace std;
		vector<int> one(1000);
		vector<int> two(1000);
		iota(one.begin(), one.end(), 10);
		iota(two.begin(), two.end(), 10);
		auto val = parallel::equal(one.begin(), one.end(), two.begin());

		BOOST_CHECK(val);
	}
	BOOST_AUTO_TEST_CASE(equal_test2) {
		using namespace std;
		vector<int> one(1000);
		vector<int> two(1000);
		iota(one.begin(), one.end(), 10);
		iota(two.begin(), two.end(), 10);
		auto val = parallel::equal(one.begin(), one.end(), two.begin(), two.end());

		BOOST_CHECK(val);
	}

	BOOST_AUTO_TEST_CASE(equal_test3) {
		using namespace std;
		vector<int> one(1000);
		vector<int> two(1000);
		iota(one.begin(), one.end(), 10);
		iota(two.begin(), two.end(), 10);
		auto val = parallel::equal(one.begin(), one.end(), two.begin(),
				[](int a, int b)->bool {return a==b;});
		BOOST_CHECK(val);
	}
	BOOST_AUTO_TEST_CASE(equal_test4) {
		using namespace std;
		vector<int> one(1000);
		vector<int> two(1000);
		iota(one.begin(), one.end(), 10);
		iota(two.begin(), two.end(), 10);
		auto val = parallel::equal(one.begin(), one.end(), two.begin(), two.end(),
				[](int a, int b)->bool {return a==b;});

		BOOST_CHECK(val);
	}
	BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(MinMax)
	BOOST_AUTO_TEST_CASE(min_test1) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 100);

		auto val = parallel::min_element(one.begin(), one.end());

		BOOST_CHECK(*val == 100);
	}

	BOOST_AUTO_TEST_CASE(min_test2) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 100);

		auto val = parallel::min_element(one.begin(), one.end(),
				[]( int i , int j)->bool {return i<j;});

		BOOST_CHECK(*val == 100);
	}
	BOOST_AUTO_TEST_CASE(max_test1) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 100);

		auto val = parallel::max_element(one.begin(), one.end());

		BOOST_CHECK(*val == 1099);
	}

	BOOST_AUTO_TEST_CASE(max_test2) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 100);

		auto val = parallel::max_element(one.begin(), one.end(),
				[]( int i , int j)->bool {return i<j;});

		BOOST_CHECK(*val == 1099);
	}

	BOOST_AUTO_TEST_CASE(minmax_test1) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 100);

		auto val = parallel::minmax_element(one.begin(), one.end());

		BOOST_CHECK(*(val.first) == 100);
		BOOST_CHECK(*(val.second) == 1099);
	}
	BOOST_AUTO_TEST_CASE(minmax_test2) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 100);

		auto val = parallel::minmax_element(one.begin(), one.end(),
				[]( int i , int j)->bool {return i<j;});

		BOOST_CHECK(*(val.first) == 100);
		BOOST_CHECK(*(val.second) == 1099);
	}
	BOOST_AUTO_TEST_SUITE_END()

	/*init_unit_test_suite(int argc, char**argv){
	 return 0;
	 }*/
//int unit_test_main( init_unit_test_func init_func, int argc, char* argv[] ){ return 0;}
