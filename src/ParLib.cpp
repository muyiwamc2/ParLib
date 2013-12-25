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

	BOOST_AUTO_TEST_CASE( fill_test ) {
			int val =20;
			std::vector<int> one(1000);
			std::vector<double> two(1000);
			std::fill(one.begin(),one.begin()+300, val);
			parallel::fill(two.begin(), two.begin()+300,val);

			BOOST_CHECK(std::equal(one.begin(),one.end(),two.begin()));
		}

	BOOST_AUTO_TEST_CASE( fill_n_test1 ) {
				int val =20;
				int val2=99;
				std::vector<int> one(100);
				std::vector<int> two(100);
				auto it1 =std::fill_n(one.begin(),val2, val);
				auto it2 =parallel::fill_n(two.begin(),val2,val);
				it1++;
				it2++;
				//for(auto &i:two)std::cout<<i<<std::endl;
				BOOST_CHECK(std::equal(one.begin(),one.end(),two.begin()));
			}
	BOOST_AUTO_TEST_CASE( generate_test1 ) {

				std::vector<int> one(1000);
				std::vector<double> two(1000);
				std::iota(one.begin(),one.end(),500);
				std::iota(two.begin(),two.end(),500);
				std::srand(0);
				std::function<int(void)>  f =[&](void)->int{return 9;} ;
				std::generate(one.begin(),one.end(), f);
				parallel::generate(two.begin(), two.end(),f);

				BOOST_CHECK(std::equal(one.begin(),one.end(),two.begin()));
			}

		BOOST_AUTO_TEST_CASE( generate_n_test1 ) {
					std::size_t val=500;
					std::vector<int> one(1000);
					std::vector<int> two(1000);
					std::function<int(void)>  f =[&](void)->int{return 9;} ;
					auto it1 =std::generate_n(one.begin(),val, f);

					auto it2 =parallel::generate_n(two.begin(),val,f);
					//it1++;
					//it2++;
					//for(auto &i:two)std::cout<<i<<std::endl;
					BOOST_CHECK(std::equal(one.begin(),one.end(),two.begin()) && *it1==*it2);
				}
	BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(ForEach)
	BOOST_AUTO_TEST_CASE( foreach_test1) {
		using namespace std;
		vector<int> one(1000);
		vector<int> two(1000);
		iota(one.begin(), one.end(), 1);
		iota(two.begin(), two.end(), 1);
		//function<void(int&)> op = [&](int &k)->void{ one[k-1]= k*30; };
		std::for_each(one.begin(), one.end(), [&](int &k)->void {one[k-1]= k*30;});
		parallel::for_each(two.begin(), two.end(), [&](int &k)->void {two[k-1]= k*30;});
		BOOST_CHECK(std::equal(one.begin(),one.end(),two.begin()));

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
	BOOST_AUTO_TEST_SUITE_END()BOOST_AUTO_TEST_SUITE
(Adjacent_Difference)
	BOOST_AUTO_TEST_CASE( adjacent_difference_test1) {
		using namespace std;
		vector<int> one(1000);
		vector<int> two(1000);
		vector<int> three(1000);
		iota(one.begin(), one.end(), 1);
		int j;
		for(auto &i : one) {
			++j;
			i += j;
		}
		std::adjacent_difference(one.begin(), one.end(), two.begin());

		parallel::adjacent_difference(one.begin(), one.end(), three.begin());
		//for(auto k: three)cout<<k<<std::endl;
		BOOST_CHECK(std::equal(two.begin(), two.end(), three.begin()));
	}
	BOOST_AUTO_TEST_CASE( adjacent_difference_test2) {
		using namespace std;
		vector<int> one(1000);
		vector<int> two(1000);
		vector<int> three(1000);
		iota(one.begin(), one.end(), 1);
		int j;
		//function<int(int,int)> op = [&](int &a, int &b)->int{ return b-a;} ;
		for(auto &i : one) {
			++j;
			i = i + j;
		}
		std::adjacent_difference(one.begin(), one.end(), two.begin(),
				[](int a, int b)->int {return a-b;});

		parallel::adjacent_difference(one.begin(), one.end(), three.begin(),
				[](int a, int b)->int {return a-b;});
		//for(auto k: three)cout<<k<<std::endl;
		BOOST_CHECK(std::equal(two.begin(), two.end(), three.begin()));
	}

	BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(Partial_Sum)
	BOOST_AUTO_TEST_CASE( partial_sum_test1) {
		using namespace std;
		vector<int> one(100);
		vector<int> two(100);
		vector<int> three(100);
		iota(one.begin(), one.end(), 1);

		std::partial_sum(one.begin(), one.end(), two.begin());

		parallel::partial_sum(one.begin(), one.end(), three.begin());
		//for(auto k: three)cout<<k<<std::endl;
		BOOST_CHECK(std::equal(two.begin(), two.end(), three.begin()));
	}
	BOOST_AUTO_TEST_CASE( partial_sum_test2) {
		using namespace std;
		vector<int> one(1000);
		vector<int> two(1000);
		vector<int> three(1000);
		iota(one.begin(), one.end(), 1);

		std::partial_sum(one.begin(), one.end(), two.begin(), [](int a, int b)->int {return a +b;});

		parallel::partial_sum(one.begin(), one.end(), three.begin(),
				[](int a, int b)->int {return a +b;});
		//for(auto k: three)cout<<k<<std::endl;
		BOOST_CHECK(std::equal(two.begin(), two.end(), three.begin()));
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
	BOOST_AUTO_TEST_CASE( search_test1) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		vector<int> two(20);
		iota(two.begin(), two.end(), 456);
		auto val1 = std::search(one.begin(), one.end(), two.begin(), two.end());
		auto val2 = parallel::search(one.begin(), one.end(), two.begin(), two.end());
		//cout<<*val1<<endl;
		//cout<<*val2<<endl;
		BOOST_CHECK(val1 == val2);
	}
	BOOST_AUTO_TEST_CASE( search_test2) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		vector<int> two(20);
		iota(two.begin(), two.end(), 456);
		std::function<bool(int, int)> op = [&](int v1, int v2) {return v1==v2;};
		auto val1 = std::search(one.begin(), one.end(), two.begin(), two.end(), op);
		auto val2 = parallel::search(one.begin(), one.end(), two.begin(), two.end(), op);
		//cout<<*val1<<endl;
		//cout<<*val2<<endl;
		BOOST_CHECK(val1 == val2);
	}
	BOOST_AUTO_TEST_CASE( search_n_test1) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		int val = 173;
		int vl = 20;
		fill(one.begin() + 456, one.begin() + 456 + 21, 173);
		//std::function<bool(int,int)>  op =[&](int v1, int v2){return v1==v2 ;};
		auto val1 = std::search_n(one.begin(), one.end(), vl, val);
		auto val2 = parallel::search_n(one.begin(), one.end(), vl, val);

		BOOST_CHECK(val1 == val2);
	}

	BOOST_AUTO_TEST_CASE( search_n_test2) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		int val = 173;
		int vl = 20;
		fill(one.begin() + 456, one.begin() + 456 + 21, 173);
		std::function<bool(int, int)> op = [&](int v1, int v2) {return v1==v2;};
		auto val1 = std::search_n(one.begin(), one.end(), vl, val, op);
		auto val2 = parallel::search_n(one.begin(), one.end(), vl, val, op);

		BOOST_CHECK(val1 == val2);
	}
	BOOST_AUTO_TEST_CASE( find_end_test1) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		vector<int> two(20);
		iota(two.begin(), two.end(), 456);
		auto val1 = std::find_end(one.begin(), one.end(), two.begin(), two.end());
		auto val2 = parallel::find_end(one.begin(), one.end(), two.begin(), two.end());
		//cout<<*val1<<endl;
		//cout<<*val2<<endl;

		BOOST_CHECK(val1 == val2);
	}
	BOOST_AUTO_TEST_CASE( find_end_test2) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		vector<int> two(20);
		iota(two.begin(), two.end(), 456);
		std::function<bool(int, int)> op = [&](int v1, int v2) {return v1==v2;};
		auto val1 = std::find_end(one.begin(), one.end(), two.begin(), two.end(), op);
		auto val2 = parallel::find_end(one.begin(), one.end(), two.begin(), two.end(), op);
		//cout<<*val1<<endl;
		//cout<<*val2<<endl;

		BOOST_CHECK(val1 == val2);
	}
	BOOST_AUTO_TEST_CASE( mismatch_test1) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		vector<int> two(1000);
		iota(two.begin(), two.end(), 1);
		two[877] = 987;
		auto val1 = std::mismatch(one.begin(), one.end(), two.begin());
		auto val2 = parallel::mismatch(one.begin(), one.end(), two.begin());
		//cout<<*val1<<endl;
		//cout<<*val2<<endl;

		BOOST_CHECK(val1 == val2);
	}
	BOOST_AUTO_TEST_CASE( mismatch_test2) {
		using namespace std;
		vector<int> one(1000);
		iota(one.begin(), one.end(), 1);
		vector<int> two(1000);
		iota(two.begin(), two.end(), 1);
		std::function<bool(int, int)> op = [&](int v1, int v2) {return v1==v2;};
		two[877] = 987;
		auto val1 = std::mismatch(one.begin(), one.end(), two.begin(), op);
		auto val2 = parallel::mismatch(one.begin(), one.end(), two.begin(), op);
		//cout<<*val1<<endl;
		//cout<<*val2<<endl;

		BOOST_CHECK(val1 == val2);
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

	BOOST_AUTO_TEST_SUITE(Copy)
	BOOST_AUTO_TEST_CASE(copy_test1) {
				using namespace std;
				vector<int> one(100);
				iota(one.begin(), one.end(), 100);

				std::vector<int> three(501);
				std::vector<int> four(501);
				auto val1 = std::copy(one.begin(),one.end(),three.begin());
				//std::cout<<*val1<<std::endl;

				auto val2 = parallel::copy(one.begin(),one.end(),four.begin());
				auto val = std::equal(three.begin(), three.end(), four.begin(),
						[](int a, int b)->bool {return a==b;});
				//std::cout<<*val2<<std::endl;
				//std::cout<<val<<std::endl;
				//std::cout<<"the size of one is:"<<one.size()<<std::endl;

				BOOST_CHECK(val==true  && *(val1-1) == *(val2-1));
			}
	BOOST_AUTO_TEST_CASE(copy_n_test1) {
					using namespace std;
					vector<int> one(1000);
					iota(one.begin(), one.end(), 100);

					std::vector<int> three(1000);
					std::vector<int> four(1000);
					auto val1 = std::copy_n(one.begin(),800,three.begin());
					//std::cout<<*val1<<std::endl;

					auto val2 = parallel::copy_n(one.begin(),800,four.begin());
					auto val = std::equal(three.begin(), three.end(), four.begin(),
							[](int a, int b)->bool {return a==b;});
					//std::cout<<*val2<<std::endl;
					//std::cout<<val<<std::endl;
					//std::cout<<"the size of one is:"<<one.size()<<std::endl;

					BOOST_CHECK(val==true  && *(val1-1) == *(val2-1));
				}
		BOOST_AUTO_TEST_CASE(copy_if_test1) {
			using namespace std;
			vector<int> one(100);
			iota(one.begin(), one.end(), 100);

			std::vector<int> three(501);
			std::vector<int> four(501);
			std::function<bool(int)> filt = [](int k)->bool{return k %2;};
			auto val1 = std::copy_if(one.begin(),one.end(),three.begin(),filt);
			//std::cout<<*val1<<std::endl;
			iota(one.begin(), one.end(), 100);
			auto val2 = parallel::copy_if(one.begin(),one.end(),four.begin(),filt);
			auto val = std::equal(three.begin(), three.end(), four.begin(),
					[](int a, int b)->bool {return a==b;});
			//std::cout<<*val2<<std::endl;
			//std::cout<<val<<std::endl;
			//std::cout<<"the size of one is:"<<one.size()<<std::endl;

			BOOST_CHECK(val==true && (*val1== *val2));
		}
	BOOST_AUTO_TEST_SUITE_END()

	BOOST_AUTO_TEST_SUITE(replace)
			BOOST_AUTO_TEST_CASE(replace_test1) {
				using namespace std;
				vector<int> one(1000);
				vector<int> two(1000);
				iota(one.begin(), one.end(), 100);
				iota(two.begin(), two.end(), 100);
				int val1=455;
				int val2=5989;
				std::replace(one.begin(),one.end(),val1,val2);
				parallel::replace(two.begin(),two.end(),val1,val2);
				auto val = std::equal(one.begin(),one.end(),two.begin());
				BOOST_CHECK(val);
			}
	BOOST_AUTO_TEST_CASE(replace_if_test1) {
					using namespace std;
					vector<int> one(1000);
					vector<int> two(1000);
					int val2 =6945;
					iota(one.begin(), one.end(), 100);
					iota(two.begin(), two.end(), 100);
					std::function<bool(int)> filt = [](int k)->bool{return k %2;};
					std::replace_if(one.begin(),one.end(),filt,val2);
					parallel::replace_if(two.begin(),two.end(),filt,val2);
					auto val = std::equal(one.begin(),one.end(),two.begin());
					BOOST_CHECK(val);
				}

	BOOST_AUTO_TEST_CASE(replace_copy_test1) {
					using namespace std;
					vector<int> one(1000);
					vector<int> two(1000);
					vector<int> three(1000);
					int val1=455;
					int val2=5989;
					iota(one.begin(), one.end(), 100);
					auto ans2 =std::replace_copy(one.begin(),one.end(),two.begin(),val1,val2);
					auto ans1 =parallel::replace_copy(one.begin(),one.end(),three.begin(),val1,val2);
					auto val = std::equal(two.begin(),two.end(),three.begin());
					BOOST_CHECK(val && *(ans1-1)==*(ans2-1));
				}

	BOOST_AUTO_TEST_CASE(replace_copy_if_test1) {
						using namespace std;
						vector<int> one(1000);
						vector<int> two(1000);
						vector<int> three(1000);
						iota(one.begin(), one.end(), 100);
						int val2=5989;
						std::function<bool(int)> filt = [](int k)->bool{return k %2;};
						auto ans2 =std::replace_copy_if(one.begin(),one.end(),two.begin(),filt,val2);
						auto ans1 =parallel::replace_copy_if(one.begin(),one.end(),three.begin(),filt,val2);
						auto val = std::equal(two.begin(),two.end(),three.begin());
						BOOST_CHECK(val && *(ans1-1)==*(ans2-1));
					}
		BOOST_AUTO_TEST_SUITE_END()

	/*init_unit_test_suite(int argc, char**argv){
	 return 0;
	 }*/
//int unit_test_main( init_unit_test_func init_func, int argc, char* argv[] ){ return 0;}
