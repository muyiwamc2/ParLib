/*
 * ParLib.h
 *
 *  Created on: Nov 12, 2013
 *      Author: muyiwa
 */

#ifndef PARLIB_H_
#define PARLIB_H_

/*
 * ParLib.h
 *
 *  Created on: Nov 10, 2013
 *      Author: muyiwa
 */

#include<algorithm>
#include<thread>
#include<functional>
#include<iterator>
#include<future>
#include<utility>
#include<condition_variable>
#include<iostream>

namespace parallel {
	enum class ThreadTypes {
		standard, async
	};
	template<typename InputIt, unsigned int blksz = 25, ThreadTypes tT = ThreadTypes::standard>
	class LaunchPolicies {
		public:
			ThreadTypes tTypes;
			unsigned long length;
			unsigned long hardware_threads = std::thread::hardware_concurrency();
			unsigned long min_per_thread;
			unsigned long max_threads;
			unsigned long num_threads;
			unsigned long block_size;
			void SetLaunchPolicies(InputIt beg, InputIt end) {
				tTypes = tT;
				length = std::distance(beg, end);
				hardware_threads = std::thread::hardware_concurrency();
				min_per_thread = blksz;
				max_threads = (length + min_per_thread) / min_per_thread;
				;
				num_threads = std::min(hardware_threads != 0 ? hardware_threads : 2, max_threads);
				block_size = length / num_threads;
			}
	};
	template<typename InputIt>
	class LaunchPolicies<InputIt, 25, ThreadTypes::standard> {
		public:
			ThreadTypes tTypes;
			unsigned long length;
			unsigned long hardware_threads;
			unsigned long min_per_thread;
			unsigned long max_threads;
			unsigned long num_threads;
			unsigned long block_size;
			void SetLaunchPolicies(InputIt beg, InputIt end) {
				tTypes = ThreadTypes::standard;
				length = std::distance(beg, end);
				hardware_threads = std::thread::hardware_concurrency();
				min_per_thread = 25;
				max_threads = (length + min_per_thread) / min_per_thread;
				;
				num_threads = std::min(hardware_threads != 0 ? hardware_threads : 2, max_threads);
				block_size = length / num_threads;
			}
	};

	template<typename InputIt, typename UnaryFunction>
	struct foreach_block {
			void operator ()(InputIt beg, InputIt end, UnaryFunction f, std::input_iterator_tag) {
				std::for_each(beg, end, f);
			}
	};

	template<typename InputIt, typename UnaryFunction, typename Tpolicy = LaunchPolicies<InputIt> >
	UnaryFunction for_each(InputIt beg, InputIt end, UnaryFunction f) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return f;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		InputIt block_start = beg;
		InputIt block_end = beg;
		for(int i; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(foreach_block<InputIt, UnaryFunction>(), block_start,
					block_end, f, typename std::iterator_traits<InputIt>::iterator_category());

			block_start = block_end;
		}
		foreach_block<InputIt, UnaryFunction>()(block_start, end, f);
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join),
				typename std::iterator_traits<InputIt>::iterator_category());

		return f;
	}

	template<typename InputIt, typename OutputIt, typename UnaryOperator>
	struct transform_block {
			void operator()(InputIt first1, InputIt last1, OutputIt result, UnaryOperator op,
							std::input_iterator_tag) {
				std::transform(first1, last1, result, op);

			}
	};
	template<typename InputIt, typename InputIt2, typename OutputIt, typename BinaryOperator>
	struct transform_block2 {
			void operator()(InputIt first1, InputIt last1, InputIt2 first2, OutputIt result,
							BinaryOperator op, std::input_iterator_tag, std::input_iterator_tag) {
				std::transform(first1, last1, first2, result, op);

			}
	};

	template<typename InputIt, typename OutputIt, typename UnaryOperator,
			typename Tpolicy = LaunchPolicies<InputIt>>
	OutputIt transform(InputIt beg, InputIt end, OutputIt result, UnaryOperator op) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return result;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		OutputIt outblock_start = result;
		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(transform_block<InputIt, OutputIt, UnaryOperator>(),
					block_start, block_end, outblock_start, op,
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(outblock_start, Tp.block_size);
		}
		transform_block<InputIt, OutputIt, UnaryOperator>()(block_start, last, outblock_start, op,
				typename std::iterator_traits<InputIt>::iterator_category());

		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return result;
	}

	template<typename InputIt, typename InputIt2, typename OutputIt, typename BinaryOperator,
			class Tpolicy = LaunchPolicies<InputIt>>
	OutputIt transform(InputIt beg1, InputIt end1, InputIt2 beg2, OutputIt result,
						BinaryOperator op) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return result;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		InputIt block_start1 = beg1;
		InputIt2 block_start2 = beg2;
		InputIt block_end1 = beg1;
		InputIt last1 = end1;

		OutputIt outblock_start = result;
		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end1, Tp.block_size);
			threads[i] = std::thread(
					transform_block2<InputIt, InputIt2, OutputIt, BinaryOperator>(), block_start1,
					block_end1, block_start2, outblock_start, op,
					typename std::iterator_traits<InputIt>::iterator_category(),
					typename std::iterator_traits<InputIt2>::iterator_category());

			block_start1 = block_end1;
			std::advance(block_start2, Tp.block_size);
			std::advance(outblock_start, Tp.block_size);
		}
		transform_block2<InputIt, InputIt2, OutputIt, BinaryOperator>()(block_start2, last1,
				block_start2, outblock_start, op,
				typename std::iterator_traits<InputIt>::iterator_category(),
				typename std::iterator_traits<InputIt2>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return result;
	}

	template<typename InputIt, typename T>
	struct accumulate_block {
			T operator()(InputIt beg, InputIt end, T &ret, std::input_iterator_tag) {

				ret = std::accumulate(beg, end, ret);
				return ret;

			}
	};

	template<typename InputIt, typename T, typename BinaryOperator>
	struct accumulate_block2 {
			T operator()(InputIt beg, InputIt end, T &ret, BinaryOperator op,
							std::input_iterator_tag) {

				ret = std::accumulate(beg, end, ret, op);
				return ret;

			}
	};

	template<typename InputIt, typename T, typename Tpolicy = LaunchPolicies<InputIt>>
	T accumulate(InputIt beg, InputIt end, T init) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return init;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < T > output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(accumulate_block<InputIt, T>(), block_start, block_end,
					std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		accumulate_block<InputIt, T>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return std::accumulate(output.begin(), output.end(), init);
	}

	template<typename InputIt, typename T, typename BinaryOperator,
			typename Tpolicy = LaunchPolicies<InputIt>>
	T accumulate(InputIt beg, InputIt end, T init, BinaryOperator op) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return init;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < T > output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(accumulate_block2<InputIt, T, BinaryOperator>(), block_start,
					block_end, std::ref(output[i]), op,
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		accumulate_block2<InputIt, T, BinaryOperator>()(block_start, last,
				std::ref(output[Tp.num_threads - 1]), op,
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return std::accumulate(output.begin(), output.end(), init, op);
	}

	template<typename InputIt, typename T, typename UnaryPred>
	struct accumulate_if_block {
			T operator()(InputIt beg, InputIt end, T& ret, UnaryPred p, std::input_iterator_tag) {
				for(; beg != end; ++beg) {
					if(p(*beg))
						ret = ret + *beg;
				}
				return ret;
			}
	};

	template<typename InputIt, typename T, typename UnaryPred, typename BinaryOperator>
	struct accumulate_if_block2 {
			T operator()(InputIt beg, InputIt end, T& ret, UnaryPred p, BinaryOperator op,
							std::input_iterator_tag) {
				for(; beg != end; ++beg) {
					if(p(*beg))
						ret = op(ret, *beg);
				}
				return ret;
			}
	};
	template<typename InputIt, typename T, typename UnaryPred, typename Tpolicy = LaunchPolicies<
			InputIt>>
	T accumulate_if(InputIt beg, InputIt end, T init, UnaryPred p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return init;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < T > output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(accumulate_if_block<InputIt, T, UnaryPred>(), block_start,
					block_end, std::ref(output[i]), p,
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		accumulate_if_block<InputIt, T, UnaryPred>()(block_start, last,
				std::ref(output[Tp.num_threads - 1]), p,
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return std::accumulate(output.begin(), output.end(), init);
	}

	template<typename InputIt, typename T, typename UnaryPred, typename BinaryOperator,
			typename Tpolicy = LaunchPolicies<InputIt>>
	T accumulate_if(InputIt beg, InputIt end, T init, UnaryPred p, BinaryOperator op) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return init;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < T > output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(accumulate_if_block2<InputIt, T, UnaryPred, BinaryOperator>(),
					block_start, block_end, std::ref(output[i]), p, op,
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		accumulate_if_block2<InputIt, T, UnaryPred, BinaryOperator>()(block_start, last,
				std::ref(output[Tp.num_threads - 1]), p, op,
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return std::accumulate(output.begin(), output.end(), init, op);
	}

	template<typename InputIt, typename T>
	struct find_block {
			void operator()(InputIt beg, InputIt end, T const & vl, std::pair<InputIt, bool>& ret,
							std::input_iterator_tag) {
				InputIt res = std::find(beg, end, vl);
				if(res == end) {
					ret.first = end;
					ret.second = false;

				}
				else {
					ret.first = res;
					ret.second = true;
				}
			}
	};

	template<typename InputIt, typename T, typename Tpolicy = LaunchPolicies<InputIt> >
	InputIt find(InputIt beg, InputIt end, const T & val) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return end;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<InputIt, bool>> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(find_block<InputIt, T>(), block_start, block_end, val,
					std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		find_block<InputIt, T>()(block_start, last, val, std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::find_if(output.begin(), output.end(),
				[](std::pair<InputIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}

	template<typename InputIt, typename UnaryPredicate>
	struct find_if_block {
			void operator()(InputIt beg, InputIt end, UnaryPredicate p,
							std::pair<InputIt, bool>& ret, std::input_iterator_tag) {
				InputIt res = std::find_if(beg, end, p);
				if(res == end) {
					ret.first = end;
					ret.second = false;

				}
				else {
					ret.first = res;
					ret.second = true;
				}
			}
	};

	template<typename InputIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<InputIt> >
	InputIt find_if(InputIt beg, InputIt end, UnaryPredicate p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return end;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<InputIt, bool>> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(find_if_block<InputIt, UnaryPredicate>(), block_start,
					block_end, p, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		find_if_block<InputIt, UnaryPredicate>()(block_start, last, p,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::find_if(output.begin(), output.end(),
				[](std::pair<InputIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}

	template<typename InputIt, typename UnaryPredicate>
	InputIt find_if_not(InputIt beg, InputIt end, UnaryPredicate p) {
		return find_if(beg, end, std::not1(p));
	}

	template<typename InputIt, typename T>
	struct count_block {
			typename std::iterator_traits<InputIt>::difference_type operator ()(
					InputIt beg, InputIt end, const T &val,
					typename std::iterator_traits<InputIt>::difference_type &ret,
					std::input_iterator_tag) {
				ret = std::count(beg, end, val);
				return ret;
			}
	};

	template<typename InputIt, typename T, typename Tpolicy = LaunchPolicies<InputIt> >
	typename std::iterator_traits<InputIt>::difference_type count(InputIt beg, InputIt end,
																	const T &val) {
		Tpolicy Tp;
		typename std::iterator_traits<InputIt>::difference_type ret = 0;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return ret;
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<typename std::iterator_traits<InputIt>::difference_type> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(count_block<InputIt, T>(), block_start, block_end, val,
					std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		count_block<InputIt, T>()(block_start, last, val, std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::accumulate(output.begin(), output.end(), ret);
		return ans;
	}

	template<typename InputIt, typename UnaryPredicate>
	struct count_if_block {
			typename std::iterator_traits<InputIt>::difference_type operator ()(
					InputIt beg, InputIt end, UnaryPredicate p,
					typename std::iterator_traits<InputIt>::difference_type &ret,
					std::input_iterator_tag) {
				ret = std::count_if(beg, end, p);
				return ret;
			}
	};

	template<typename InputIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<InputIt> >
	typename std::iterator_traits<InputIt>::difference_type count_if(InputIt beg, InputIt end,
																		UnaryPredicate p) {
		Tpolicy Tp;
		typename std::iterator_traits<InputIt>::difference_type ret = 0;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return ret;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<typename std::iterator_traits<InputIt>::difference_type> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(count_if_block<InputIt, UnaryPredicate>(), block_start,
					block_end, p, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		count_if_block<InputIt, UnaryPredicate>()(block_start, last, p,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::accumulate(output.begin(), output.end(), ret);
		return ans;
	}

	template<typename InputIt, typename UnaryPredicate>
	bool all_of(InputIt first, InputIt last, UnaryPredicate p) {
		return find_if_not(first, last, p) == last;
	}
	template<typename InputIt, typename UnaryPredicate>
	bool any_of(InputIt first, InputIt last, UnaryPredicate p) {
		return find_if(first, last, p) != last;
	}
	template<typename InputIt, typename UnaryPredicate>
	bool none_of(InputIt first, InputIt last, UnaryPredicate p) {
		return find_if(first, last, p) == last;
	}

	template<typename InputIt1, typename InputIt2>
	struct equal_block {
			bool operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, bool &retval,
							std::input_iterator_tag) {
				retval = std::equal(beg1, end1, beg2);
				return retval;
			}
	};

	template<typename InputIt, typename InputIt2, typename Tpolicy = LaunchPolicies<InputIt> >

	bool equal(InputIt beg1, InputIt end1, InputIt2 beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		// test to make sure they are at least as long
		{

			InputIt end2 = std::advance(beg2, Tp.length - 1);
			InputIt testend1 = std::advance(beg1, Tp.length - 1);
			try {
				if(*end2 != *testend1)
					return false;
			}
			catch(...) {

				return false; //failed.
			}
			try {
				if((Tp.length == 0) && *beg2) {
					int t = 5;
					t++;
				}

			}
			catch(...) {
				// if both failed, then it means the both containers are empty return true
				return true;
			}
		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<bool> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(equal_block<InputIt, InputIt2>(), block_start, block_end,
					block_start2, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}
		equal_block<InputIt, InputIt2>()(block_start, last, block_start2,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::all_of(output.begin(), output.end(), [](bool b)->bool {return b;});
		return ans;
	}

	template<typename InputIt1, typename InputIt2, typename BinaryPredicate>
	struct equal_block2 {
			bool operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, BinaryPredicate p,
							bool &retval, std::input_iterator_tag) {
				retval = std::equal(beg1, end1, beg2, p);
				return retval;
			}
	};

	template<typename InputIt, typename InputIt2, typename BinaryPredicate,
			typename Tpolicy = LaunchPolicies<InputIt> >

	bool equal(InputIt beg1, InputIt end1, InputIt2 beg2, BinaryPredicate p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		// test to make sure they are at least as long
		{

			InputIt end2 = std::advance(beg2, Tp.length - 1);
			InputIt testend1 = std::advance(beg1, Tp.length - 1);
			try {
				if(!p(*end2, *testend1))
					return false;
			}
			catch(...) {

				return false; //failed.
			}
			try {
				if((Tp.length == 0) && *beg2) {
					int t = 5;
					t++;
				}

			}
			catch(...) {
				// if both failed, then it means the both containers are empty return true
				return true;
			}
		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<bool> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(equal_block2<InputIt, InputIt2, BinaryPredicate>(),
					block_start, block_end, block_start2, p, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}
		equal_block2<InputIt, InputIt2, BinaryPredicate>()(block_start, last, block_start2, p,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::all_of(output.begin(), output.end(), [](bool b)->bool {return b;});
		return ans;
	}

	template<typename InputIt1, typename InputIt2, typename BinaryPredicate>
	struct equal_block3 {
			bool operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, InputIt2 end2,
							BinaryPredicate p, bool &retval, std::input_iterator_tag) {
				retval = std::equal(beg1, end1, beg2, end2, p);
				return retval;
			}
	};

	template<typename InputIt, typename InputIt2, typename BinaryPredicate,
			typename Tpolicy = LaunchPolicies<InputIt> >

	bool equal(InputIt beg1, InputIt end1, InputIt2 beg2, InputIt2 end2, BinaryPredicate p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(std::distance(beg1, end1) != std::distance(beg2, end2))
			return false;

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<bool> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(equal_block2<InputIt, InputIt2, BinaryPredicate>(),
					block_start, block_end, block_start2, p, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}
		equal_block2<InputIt, InputIt2, BinaryPredicate>()(block_start, last, block_start2, p,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::all_of(output.begin(), output.end(), [](bool b)->bool {return b;});
		return ans;
	}

	template<typename ForwardIt>
	struct max_element_block {
			void operator()(ForwardIt beg, ForwardIt end,std::pair<	ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retval,
							std::forward_iterator_tag) {
				retval.first = *(std::max_element(beg, end));
				retval.second = *retval.first;
			}
	};

	template<typename ForwardIt,
				typename Tpolicy = LaunchPolicies<ForwardIt> >


		ForwardIt max_element(ForwardIt beg, ForwardIt end) {
			Tpolicy Tp;
			Tp.SetLaunchPolicies(beg, end);
			if(!Tp.length)
				return beg;

			std::vector < std::thread > threads(Tp.num_threads - 1);
			std::vector<std::pair< ForwardIt,typename std::iterator_traits<ForwardIt>::value_type> > output(Tp.num_threads);
			ForwardIt block_start = beg;
			ForwardIt block_end = beg;

			ForwardIt last = end;

			for(int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(max_element_block<ForwardIt>(),
						block_start, block_end, std::ref(output[i]),
						typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			max_element_block<ForwardIt>()(block_start, last,std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

			auto ans = std::max_element(output.begin(), output.end(),[](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool{ return a.second<b.second ;});
			return ans.first;
		}


	template<typename ForwardIt, typename Comp>
		struct max_element_block2 {
				void operator()(ForwardIt beg, ForwardIt end, Comp cmp,
								std::pair<ForwardIt,typename std::iterator_traits<ForwardIt>::value_type> &retval,
								std::forward_iterator_tag) {
					retval.first = (std::max_element(beg, end,cmp));
					retval.second = *retval.first;
				}
		};

		template<typename ForwardIt, typename Comp,
					typename Tpolicy = LaunchPolicies<ForwardIt> >


			ForwardIt max_element(ForwardIt beg, ForwardIt end, Comp cmp) {
				Tpolicy Tp;
				Tp.SetLaunchPolicies(beg, end);
				if(!Tp.length)
					return beg;

				std::vector < std::thread > threads(Tp.num_threads - 1);
				std::vector<typename std::iterator_traits<ForwardIt>::value_type> output(Tp.num_threads);
				ForwardIt block_start = beg;
				ForwardIt block_end = beg;

				ForwardIt last = end;

				for(int i = 0; i < (Tp.num_threads - 1); i++) {

					std::advance(block_end, Tp.block_size);

					threads[i] = std::thread(max_element_block2<ForwardIt,Comp>(),
							block_start, block_end, std::ref(output[i]),
							typename std::iterator_traits<ForwardIt>::iterator_category());
					block_start = block_end;

				}
				max_element_block2<ForwardIt,Comp>()(block_start, last,std::ref(output[Tp.num_threads - 1]),
						typename std::iterator_traits<ForwardIt>::iterator_category());
				std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

				auto ans = std::max_element(output.begin(), output.end(),[](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
						std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool{ return comp(a.second,b.second) ;});
				return ans.first;
			}


}

#endif /* PARLIB_H_ */
