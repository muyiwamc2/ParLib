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
		if(Tp.num_threads < 2)
			return std::for_each(beg, end, f);

		std::vector < std::thread > threads(Tp.num_threads - 1);
		InputIt block_start = beg;
		InputIt block_end = beg;
		for(int i; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(foreach_block<InputIt, UnaryFunction>(), block_start,
					block_end, f, typename std::iterator_traits<InputIt>::iterator_category());

			block_start = block_end;
		}
		foreach_block<InputIt, UnaryFunction>()(block_start, end, f,
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

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
		if(Tp.num_threads < 2)
			return std::transform(beg, end, result, op);
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
		if(Tp.num_threads < 2)
			return std::transform(beg1, end1, beg2, result, op);

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
		transform_block2<InputIt, InputIt2, OutputIt, BinaryOperator>()(block_start1, last1,
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
		if(Tp.num_threads < 2)
			return std::accumulate(beg, end, init);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
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
		if(Tp.num_threads < 2)
			return std::accumulate(beg, end, init, op);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
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
		if(Tp.num_threads < 2) {
			accumulate_if_block<InputIt, T, UnaryPred> blk;
			return blk(beg, end, init, p,
					typename std::iterator_traits<InputIt>::iterator_category());
		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
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
		if(Tp.num_threads < 2) {
			accumulate_if_block2<InputIt, T, UnaryPred, BinaryOperator> blk;
			return blk(beg, end, init, p, op,
					typename std::iterator_traits<InputIt>::iterator_category());
		}
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
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

	template<typename InputIt1, typename InputIt2, typename T>
	struct inner_product_block {
			void operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, T &retval,
							std::input_iterator_tag, std::input_iterator_tag) {
				retval = std::inner_product(beg1, end1, beg2, retval);

			}
	};

	template<typename InputIt1, typename InputIt2, typename T, typename BinaryOp1,
			typename BinaryOp2>
	struct inner_product_block2 {
			void operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, T &retval, BinaryOp1 op1,
							BinaryOp2 op2, std::input_iterator_tag, std::input_iterator_tag) {
				retval = std::inner_product(beg1, end1, beg2, retval, op1, op2);

			}
	};
	template<typename InputIt, typename InputIt2, typename T, typename Tpolicy = LaunchPolicies<
			InputIt>>
	T inner_product(InputIt beg1, InputIt end1, InputIt2 beg2, T init) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return init;
		if(Tp.num_threads < 2)
			return std::inner_product(beg1, end1, beg2, init);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(inner_product_block<InputIt, InputIt2, T>(), block_start,
					block_end, block_start2, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category(),
					typename std::iterator_traits<InputIt2>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}
		inner_product_block<InputIt, InputIt2, T>()(block_start, last, block_start2,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category(),
				typename std::iterator_traits<InputIt2>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return std::accumulate(output.begin(), output.end(), init);
	}
	template<typename InputIt, typename InputIt2, typename T, typename BinaryOp1,
			typename BinaryOp2, typename Tpolicy = LaunchPolicies<InputIt>>
	T inner_product(InputIt beg1, InputIt end1, InputIt2 beg2, T init, BinaryOp1 op1,
					BinaryOp2 op2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return init;
		if(Tp.num_threads < 2)
			return std::inner_product(beg1, end1, beg2, init, op1, op2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(
					inner_product_block2<InputIt, InputIt2, T, BinaryOp1, BinaryOp2>(), block_start,
					block_end, block_start2, std::ref(output[i]), op1, op2,
					typename std::iterator_traits<InputIt>::iterator_category(),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}
		inner_product_block2<InputIt, InputIt2, T, BinaryOp1, BinaryOp2>()(block_start, last,
				block_start2, std::ref(output[Tp.num_threads - 1]), op1, op2,
				typename std::iterator_traits<InputIt>::iterator_category(),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return std::accumulate(output.begin(), output.end(), init);
	}

	template<typename InputIt, typename OutputIt>
	struct adjacent_difference_block {
			/**
			 *
			 * @param beg1 beginning of the block.
			 * @param end1 The end of the block
			 * @param beg2 the iterator to the first element in the Output container's block
			 * @param first1 the iterator the first element of the input container
			 * @param retval the return value
			 * @param making sure this works for at the least InputIterators.
			 * @return
			 */
			void operator()(InputIt beg1, InputIt end1, OutputIt beg2, InputIt first1,
							typename std::iterator_traits<InputIt>::value_type val1,
							typename std::iterator_traits<OutputIt>::value_type val2,
							OutputIt & retval, std::input_iterator_tag) {

				InputIt st = beg1;

				if(beg1 != first1) {
					st--;
					val1 = *beg1 - *st;
					val2 = val1;

				}
				retval = std::adjacent_difference(beg1, end1, beg2);
				if(beg1 != first1)
					*beg2 = val2;

			}
	};

	template<typename InputIt, typename OutputIt, typename Tpolicy = LaunchPolicies<InputIt>>
	OutputIt adjacent_difference(InputIt beg1, InputIt end1, OutputIt beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2)
			return std::adjacent_difference(beg1, end1, beg2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<OutputIt> output(Tp.num_threads);
		typedef typename std::iterator_traits<InputIt>::value_type T1;
		typedef typename std::iterator_traits<OutputIt>::value_type T2;
		std::vector<std::pair<T1, T2>> initVals(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		OutputIt block_start2 = beg2;
		InputIt last = end1;
		//initialise values and reset iterators after all is done.
		for(int i = 0; i < (Tp.num_threads - 1); i++) {
			std::advance(block_end, Tp.block_size);
			initVals[i].first = *block_start;
			initVals[i].second = *block_start2;
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}
		//set last values and reset iterators.
		initVals[Tp.num_threads - 1].first = *block_start;
		initVals[Tp.num_threads - 1].second = *block_start2;
		block_start = beg1;
		block_end = beg1;
		block_start2 = beg2;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(adjacent_difference_block<InputIt, OutputIt>(), block_start,
					block_end, block_start2, beg1, initVals[i].first, initVals[i].second,
					std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}

		adjacent_difference_block<InputIt, OutputIt>()(block_start, last, block_start2, beg1,
				initVals[Tp.num_threads - 1].first, initVals[Tp.num_threads - 1].second,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return output[Tp.num_threads - 1];
	}

	template<class InputIt, class OutputIt, typename BinaryOp>
	struct adjacent_difference_block2 {
			/**
			 *
			 * @param beg beginning of the block.
			 * @param end The end of the block
			 * @param beg2 the iterator to the first element in the Output container
			 * @param op  Binary operator used to compute A -B
			 * @param first1 the first element of the Input container.
			 * @param val1 the first value in the block of the Input container
			 * @param val2 the first value in the block for the  output container
			 * @param retval the InputIt one past the end of newbegin,end
			 * @param
			 * @return
			 */
			void operator()(InputIt beg1, InputIt end1, OutputIt beg2, BinaryOp op, InputIt first1,
							typename std::iterator_traits<InputIt>::value_type val1,
							typename std::iterator_traits<OutputIt>::value_type val2,
							OutputIt & retval, std::input_iterator_tag) {

				InputIt st = beg1;

				if(beg1 != first1) {
					st--;
					val1 = op(*beg1, *st);
					val2 = val1;

				}
				retval = std::adjacent_difference(beg1, end1, beg2, op);
				if(beg1 != first1)
					*beg2 = val2;

			}
	};

	template<typename InputIt, typename OutputIt, typename BinaryOp,
			typename Tpolicy = LaunchPolicies<InputIt>>
	OutputIt adjacent_difference(InputIt beg1, InputIt end1, OutputIt beg2, BinaryOp op) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2)
			return std::adjacent_difference(beg1, end1, beg2, op);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<OutputIt> output(Tp.num_threads);
		typedef typename std::iterator_traits<InputIt>::value_type T1;
		typedef typename std::iterator_traits<OutputIt>::value_type T2;
		std::vector<std::pair<T1, T2>> initVals(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		OutputIt block_start2 = beg2;
		InputIt last = end1;

		//initialise values and reset iterators after all is done.
		for(int i = 0; i < (Tp.num_threads - 1); i++) {
			std::advance(block_end, Tp.block_size);
			initVals[i].first = *block_start;
			initVals[i].second = *block_start2;
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}
		//set last values and reset iterators.
		initVals[Tp.num_threads - 1].first = *block_start;
		initVals[Tp.num_threads - 1].second = *block_start2;
		block_start = beg1;
		block_end = beg1;
		block_start2 = beg2;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(adjacent_difference_block2<InputIt, OutputIt, BinaryOp>(),
					block_start, block_end, block_start2, op, beg1, initVals[i].first,
					initVals[i].second, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}

		adjacent_difference_block2<InputIt, OutputIt, BinaryOp>()(block_start, last, block_start2,
				op, beg1, initVals[Tp.num_threads - 1].first, initVals[Tp.num_threads - 1].second,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		return output[Tp.num_threads - 1];
	}
	//partial sum
	template<typename InputIt, typename OutputIt>
	struct partial_sum_block {
			/**
			 * The goal of this is to do get the sum of the borders first., Then we can do the partial sums for real.On the next try.
			 * @param beg1 beginning of the block.
			 * @param end1 The end of the block
			 * @param beg2 the iterator to the first element in the Output container's block
			 * @param retval the return value of the iterator to the last element
			 * @param making sure this works for at the least InputIterators.
			 * @return
			 */
			void operator()(InputIt beg1, InputIt end1, OutputIt beg2,
							typename std::iterator_traits<InputIt>::value_type &part_sum,
							OutputIt & retval, std::input_iterator_tag) {
				typename std::iterator_traits<InputIt>::value_type sum;
				sum = *beg1;
				if(beg1 == end1) {
					retval = beg2;
					//*beg2 = sum;
					part_sum = sum;
				}
				else {

					//*beg2 = sum;

					while(++beg1 != end1) {
						sum = sum + *beg1;
						//*++beg2 = sum;
					}
					retval = ++beg2;
					part_sum = sum;
				}
				part_sum = sum;
			}
			/**
			 * Helper function to make partial sums correct.
			 * @param beg1 beginning of the input container
			 * @param end1 end of the input container
			 * @param beg2 beginning of the output container
			 * @param AddValue this is the value to be added to the partial sums to make them correct
			 * @param
			 */
			void operator()(InputIt beg1, InputIt end1, OutputIt beg2,
							typename std::iterator_traits<InputIt>::value_type &AddValue,
							std::input_iterator_tag) {

				typename std::iterator_traits<InputIt>::value_type sum;
				sum = *beg1 + AddValue;

				if(beg1 == end1) {
					//*beg2 = sum ;
				}
				else {

					*beg2 = sum;

					while(++beg1 != end1) {
						sum = sum + *beg1;
						//sum = sum + AddValue;
						*++beg2 = sum;
					}

				}

			}
	};
	template<typename InputIt, typename OutputIt, typename Tpolicy = LaunchPolicies<InputIt>>
	OutputIt partial_sum(InputIt beg1, InputIt end1, OutputIt beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2)
			return std::partial_sum(beg1, end1, beg2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<OutputIt> output(Tp.num_threads);
		std::vector<typename std::iterator_traits<InputIt>::value_type> parsum(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		OutputIt block_start2 = beg2;
		InputIt last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(partial_sum_block<InputIt, OutputIt>(), block_start, block_end,
					block_start2, std::ref(parsum[i]), std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}

		partial_sum_block<InputIt, OutputIt>()(block_start, last, block_start2,
				std::ref(parsum[Tp.num_threads - 1]), std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		//do partial sums on the partial sums from the edges to make them accurate
		std::partial_sum(parsum.begin(), parsum.end(), parsum.begin());

		//redo partial sums with threads on what is left.
		block_start = beg1;
		block_end = beg1;
		block_start2 = beg2;
		InputIt bend2 = beg1;
		std::advance(bend2, Tp.block_size);
		//redo the partial sum on the first block
		std::partial_sum(beg1, bend2, beg2);
		//move past first block since first block is always correct :)
		std::advance(block_end, Tp.block_size);
		block_start = block_end;
		std::advance(block_start2, Tp.block_size);
		//now go through the motions.
		for(int i = 1; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(partial_sum_block<InputIt, OutputIt>(), block_start, block_end,
					block_start2, std::ref(parsum[i - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}

		partial_sum_block<InputIt, OutputIt>()(block_start, last, block_start2,
				std::ref(parsum[Tp.num_threads - 2]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin() + 1, threads.end(), std::mem_fn(&std::thread::join));

		return output[Tp.num_threads - 1];
	}

	template<typename InputIt, typename OutputIt, typename BinaryOp>
	struct partial_sum_block2 {
			/**
			 * This version does not actually update the output container we use this to get the sums at the borders.
			 * @param beg1 beginning of the block.
			 * @param end1 The end of the block
			 * @param beg2 the iterator to the first element in the Output container's block
			 * @param op  Binary operator of type Ret fun(const Type1 &a, const Type2 &b);
			 The signature does not need to have const &.
			 The type Type1 must be such that an object of type iterator_traits<InputIt>::value_type
			 can be implicitly converted to Type1. The type Type2 must be such that an object of type
			 InputIt can be dereferenced and then implicitly converted to Type2.
			 The type Ret must be such that an object of type iterator_traits<InputIt>::value_type
			 can be assigned a value of type Ret.
			 * @param retval the return value of the iterator to the last element
			 * @param making sure this works for at the least InputIterators.
			 * @return
			 */
			void operator()(InputIt beg1, InputIt end1, OutputIt beg2, BinaryOp op,
							typename std::iterator_traits<InputIt>::value_type &part_sum,
							OutputIt & retval, std::input_iterator_tag) {
				typename std::iterator_traits<InputIt>::value_type sum;
				sum = *beg1;
				if(beg1 == end1) {
					retval = beg2;
					//*beg2 = sum;
					part_sum = sum;
				}
				else {

					//*beg2 = sum;

					while(++beg1 != end1) {
						sum = op(sum, *beg1);
						//*++beg2 = sum;
					}
					retval = ++beg2;
					part_sum = sum;
				}
				part_sum = sum;
			}
			/**
			 * Helper function to make partial sums correct.
			 * @param beg1 beginning of the input container
			 * @param end1 end of the input container
			 * @param beg2 beginning of the output container
			 * @param AddValue this is the value to be added to the partial sums to make them correct
			 * @param
			 */
			void operator()(InputIt beg1, InputIt end1, OutputIt beg2, BinaryOp op,
							typename std::iterator_traits<InputIt>::value_type &AddValue,
							std::input_iterator_tag) {

				typename std::iterator_traits<InputIt>::value_type sum;
				sum = op(AddValue, *beg1);
				InputIt first1 = beg1;
				OutputIt first2 = beg2;

				if(beg1 == end1) {
					//*beg2 = op(sum, AddValue);
				}
				else {

					*beg2 = sum;

					while(++beg1 != end1) {
						sum = op(sum, *beg1);
						*++beg2 = sum;
					}
					*first2 = op(AddValue, *first1);  //make the first one correct again

				}

			}
	};

	template<typename InputIt, typename OutputIt, typename BinaryOp,
			typename Tpolicy = LaunchPolicies<InputIt>>
	OutputIt partial_sum(InputIt beg1, InputIt end1, OutputIt beg2, BinaryOp op) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2)
			return std::partial_sum(beg1, end1, beg2, op);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<OutputIt> output(Tp.num_threads);
		std::vector<typename std::iterator_traits<InputIt>::value_type> parsum(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		OutputIt block_start2 = beg2;
		InputIt last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(partial_sum_block2<InputIt, OutputIt, BinaryOp>(), block_start,
					block_end, block_start2, op, std::ref(parsum[i]), std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}

		partial_sum_block2<InputIt, OutputIt, BinaryOp>()(block_start, last, block_start2, op,
				std::ref(parsum[Tp.num_threads - 1]), std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		//do partial sums on the partial sums from the edges to make them accurate
		std::partial_sum(parsum.begin(), parsum.end(), parsum.begin(), op);

		//redo partial sums with threads on what is left.
		block_start = beg1;
		block_end = beg1;
		block_start2 = beg2;
		InputIt bend2 = beg1;
		std::advance(bend2, Tp.block_size);
		//redo the partial sum on the first block
		std::partial_sum(beg1, bend2, beg2, op);
		//move past first block since first block is always correct :)
		std::advance(block_end, Tp.block_size);
		block_start = block_end;
		std::advance(block_start2, Tp.block_size);
		//now go through the motions.
		for(int i = 1; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(partial_sum_block2<InputIt, OutputIt, BinaryOp>(), block_start,
					block_end, block_start2, op, std::ref(parsum[i - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			std::advance(block_start2, Tp.block_size);
		}

		partial_sum_block2<InputIt, OutputIt, BinaryOp>()(block_start, last, block_start2, op,
				std::ref(parsum[Tp.num_threads - 2]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin() + 1, threads.end(), std::mem_fn(&std::thread::join));

		return output[Tp.num_threads - 1];
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

	template<typename ForwardIt>
	struct adjacent_find_block {
			void operator()(ForwardIt beg, ForwardIt end, std::pair<ForwardIt, bool>& ret,
							std::forward_iterator_tag) {

				ForwardIt res = std::adjacent_find(beg, end);
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
		if(Tp.num_threads < 2)
			return std::find(beg, end, val);
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

	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >
	ForwardIt adjacent_find(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return end;
		if(Tp.num_threads < 2)
			return std::adjacent_find(beg, end);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, bool>> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		ForwardIt last = end;
		ForwardIt block_end2 = block_end;
		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			block_end2 = block_end;
			block_end2++;
			threads[i] = std::thread(adjacent_find_block<ForwardIt>(), block_start, block_end2,
					std::ref(output[i]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			block_start = block_end;
		}
		adjacent_find_block<ForwardIt>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::find_if(output.begin(), output.end(),
				[](std::pair<ForwardIt, bool> v)->bool {return v.second;});
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
	template<typename InputIt, typename BinaryPredicate>
	struct adjacent_find2_block {
			void operator()(InputIt beg, InputIt end, BinaryPredicate p,
							std::pair<InputIt, bool>& ret, std::forward_iterator_tag) {
				InputIt res = std::adjacent_find(beg, end, p);
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
		if(Tp.num_threads < 2)
			return std::find_if(beg, end, p);
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

	template<typename ForwardIt, typename BinaryPredicate, typename Tpolicy = LaunchPolicies<
			ForwardIt> >
	ForwardIt adjacent_find(ForwardIt beg, ForwardIt end, BinaryPredicate p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return end;
		if(Tp.num_threads < 2)
			return std::adjacent_find(beg, end, p);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, bool>> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		ForwardIt block_end2 = beg;
		ForwardIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			block_end2 = block_end;
			block_end2++;
			threads[i] = std::thread(adjacent_find2_block<ForwardIt, BinaryPredicate>(),
					block_start, block_end2, p, std::ref(output[i]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			block_start = block_end;
		}
		adjacent_find2_block<ForwardIt, BinaryPredicate>()(block_start, last, p,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::find_if(output.begin(), output.end(),
				[](std::pair<ForwardIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}

	template<typename InputIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<InputIt>>
	InputIt find_if_not(InputIt beg, InputIt end, UnaryPredicate p) {
		return parallel::find_if<InputIt, UnaryPredicate, Tpolicy>(beg, end, std::not1(p));
	}

	template<typename ForwardIt1, typename ForwardIt2>
	struct find_first_of_block {
			ForwardIt1 operator()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
									ForwardIt2 end2, std::pair<ForwardIt1, bool>&retval,
									std::forward_iterator_tag) {
				retval.first = std::find_first_of(beg1, end1, beg2, end2);
				if(retval.first == end1)
					retval.second = false;
				else
					retval.second = true;
				return retval.first;
			}
			ForwardIt1 operator()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
									ForwardIt2 end2, std::pair<ForwardIt1, bool>&retval,
									std::input_iterator_tag) {
				retval.first = std::find_first_of(beg1, end1, beg2, end2);
				if(retval.first == end1)
					retval.second = false;
				else
					retval.second = true;
				return retval.first;
			}
	};

	template<typename ForwardIt1, typename ForwardIt2, typename Tpolicy = LaunchPolicies<ForwardIt1> >
	ForwardIt1 find_first_of(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2, ForwardIt2 end2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return end1;
		if(beg2 == end2)
			return end1;
		if(Tp.num_threads < 2)
			return std::find_first_of(beg1, end1, beg2, end2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(find_first_of_block<ForwardIt1, ForwardIt2>(), block_start,
					block_end, beg2, end2, std::ref(output[i]),
					typename std::iterator_traits<ForwardIt1>::iterator_category());
			block_start = block_end;
		}
		find_first_of_block<ForwardIt1, ForwardIt2>()(block_start, last, beg2, end2,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt1>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::find_if(output.begin(), output.end(),
				[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end1;
		else
			return ans->first;
	}

	template<typename ForwardIt1, typename ForwardIt2, typename BinaryOp>
	struct find_first_of_block2 {
			ForwardIt1 operator()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
									ForwardIt2 end2, BinaryOp op,
									std::pair<ForwardIt1, bool>&retval, std::forward_iterator_tag) {
				retval.first = std::find_first_of(beg1, end1, beg2, end2, op);
				if(retval.first == end1)
					retval.second = false;
				else
					retval.second = true;
				return retval.first;
			}
			ForwardIt1 operator()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
									ForwardIt2 end2, BinaryOp op,
									std::pair<ForwardIt1, bool>&retval, std::input_iterator_tag) {
				retval.first = std::find_first_of(beg1, end1, beg2, end2, op);
				if(retval.first == end1)
					retval.second = false;
				else
					retval.second = true;
				return retval.first;
			}
	};

	template<typename ForwardIt1, typename ForwardIt2, typename BinaryOp,
			typename Tpolicy = LaunchPolicies<ForwardIt1> >
	ForwardIt1 find_first_of(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2, ForwardIt2 end2,
								BinaryOp op) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return end1;
		if(beg2 == end2)
			return end1;
		if(Tp.num_threads < 2)
			return std::find_first_of(beg1, end1, beg2, end2, op);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(find_first_of_block2<ForwardIt1, ForwardIt2, BinaryOp>(),
					block_start, block_end, beg2, end2, op, std::ref(output[i]),
					typename std::iterator_traits<ForwardIt1>::iterator_category());
			block_start = block_end;
		}
		find_first_of_block2<ForwardIt1, ForwardIt2, BinaryOp>()(block_start, last, beg2, end2, op,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt1>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::find_if(output.begin(), output.end(),
				[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end1;
		else
			return ans->first;
	}
	template<typename ForwardIt1, typename ForwardIt2>
	struct search_block {
			ForwardIt1 operator ()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
									ForwardIt2 end2, std::pair<ForwardIt1, bool> &ret,
									std::forward_iterator_tag) {

				ret.first = std::search(beg1, end1, beg2, end2);
				if(ret.first == end1)
					ret.second = false;
				else
					ret.second = true;
				return ret.first;
			}
	};

	template<typename ForwardIt1, typename ForwardIt2, typename BinaryPredicate>
	struct search_block2 {
			ForwardIt1 operator ()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
									ForwardIt2 end2, BinaryPredicate p,
									std::pair<ForwardIt1, bool> &ret, std::forward_iterator_tag) {

				ret.first = std::search(beg1, end1, beg2, end2, p);
				if(ret.first == end1)
					ret.second = false;
				else
					ret.second = true;
				return ret.first;
			}
	};
	template<typename ForwardIt1, typename ForwardIt2, typename Tpolicy = LaunchPolicies<ForwardIt1> >
	ForwardIt1 search(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2, ForwardIt2 end2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return end1;
		if(beg2 == end2)
			return end1;
		if(Tp.num_threads < 2)
			return std::search(beg1, end1, beg2, end2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 block_end2 = beg1;
		ForwardIt1 last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			block_end2 = block_end;
			std::advance(block_end2, std::distance(beg2, end2));
			if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
				block_end2 = last;
			threads[i] = std::thread(search_block<ForwardIt1, ForwardIt2>(), block_start,
					block_end2, beg2, end2, std::ref(output[i]),
					typename std::iterator_traits<ForwardIt1>::iterator_category());
			block_start = block_end;
		}
		search_block<ForwardIt1, ForwardIt2>()(block_start, last, beg2, end2,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt1>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::find_if(output.begin(), output.end(),
				[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end1;
		else
			return ans->first;
	}

	template<typename ForwardIt1, typename ForwardIt2, typename BinaryPredicate,
			typename Tpolicy = LaunchPolicies<ForwardIt1> >
	ForwardIt1 search(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2, ForwardIt2 end2,
						BinaryPredicate p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return end1;
		if(beg2 == end2)
			return end1;
		if(Tp.num_threads < 2)
			return std::search(beg1, end1, beg2, end2, p);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 block_end2 = beg1;
		ForwardIt1 last = end1;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			block_end2 = block_end;
			std::advance(block_end2, std::distance(beg2, end2));
			if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
				block_end2 = last;
			threads[i] = std::thread(search_block2<ForwardIt1, ForwardIt2, BinaryPredicate>(),
					block_start, block_end2, beg2, end2, p, std::ref(output[i]),
					typename std::iterator_traits<ForwardIt1>::iterator_category());
			block_start = block_end;
		}
		search_block2<ForwardIt1, ForwardIt2, BinaryPredicate>()(block_start, last, beg2, end2, p,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt1>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::find_if(output.begin(), output.end(),
				[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end1;
		else
			return ans->first;
	}
	template<typename ForwardIt, typename Size, typename T>
	struct search_n_block {
			ForwardIt operator () ( ForwardIt beg, ForwardIt end, Size count,
					const T& value ,std::pair<ForwardIt,bool> &ret,std::forward_iterator_tag)
			{

				ret.first = std::search_n(beg, end,count,value);
				if(ret.first==end)ret.second =false;
				else ret.second =true;
				return ret.first;
			}
	};
	template<typename ForwardIt, typename Size, typename T , typename BinaryPredicate>
		struct search_n_block2 {
				ForwardIt operator () ( ForwardIt beg, ForwardIt end, Size count,
						const T& value , BinaryPredicate p,std::pair<ForwardIt,bool> &ret,std::forward_iterator_tag)
				{

					ret.first = std::search_n(beg, end,count,value,p);
					if(ret.first==end)ret.second =false;
					else ret.second =true;
					return ret.first;
				}
		};
	template<typename ForwardIt, typename Size, typename T, typename Tpolicy = LaunchPolicies<
			ForwardIt> >
	ForwardIt search_n(ForwardIt beg, ForwardIt end, Size count, T& value) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return end;
		if(!count)
			return beg;

		if(Tp.num_threads < 2)
			return std::search_n(beg, end, count, value);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, bool>> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		ForwardIt block_end2 = beg;
		ForwardIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			block_end2 = block_end;
			std::advance(block_end2, static_cast<long>(count));
			if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
				block_end2 = last;
			threads[i] = std::thread(search_n_block<ForwardIt, Size, T>(), block_start, block_end2,
					count, value, std::ref(output[i]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			block_start = block_end;
		}
		search_n_block<ForwardIt, Size,T>()(block_start, last, count,value,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::find_if(output.begin(), output.end(),
				[](std::pair<ForwardIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}
	template<typename ForwardIt, typename Size, typename T, typename BinaryPredicate, typename Tpolicy = LaunchPolicies<
				ForwardIt> >
		ForwardIt search_n(ForwardIt beg, ForwardIt end, Size count, T& value, BinaryPredicate p) {
			Tpolicy Tp;
			Tp.SetLaunchPolicies(beg, end);
			if(!Tp.length)
				return end;
			if(!count)
				return beg;

			if(Tp.num_threads < 2)
				return std::search_n(beg, end, count, value,p);
			std::vector < std::thread > threads(Tp.num_threads - 1);
			std::vector<std::pair<ForwardIt, bool>> output(Tp.num_threads);
			ForwardIt block_start = beg;
			ForwardIt block_end = beg;
			ForwardIt block_end2 = beg;
			ForwardIt last = end;

			for(int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, static_cast<long>(count));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				threads[i] = std::thread(search_n_block2<ForwardIt, Size, T,BinaryPredicate>(), block_start, block_end2,
						count, value,p, std::ref(output[i]),
						typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			search_n_block2<ForwardIt, Size,T,BinaryPredicate>()(block_start, last, count,value,p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

			auto ans = std::find_if(output.begin(), output.end(),
					[](std::pair<ForwardIt, bool> v)->bool {return v.second;});
			if(ans == output.end())
				return end;
			else
				return ans->first;
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
		if(Tp.num_threads < 2)
			return std::count(beg, end, val);
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
		if(Tp.num_threads < 2)
			return std::count_if(beg, end, p);
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
	struct all_of_block {
			void operator ()(InputIt beg, InputIt end, UnaryPredicate p, int &ret,
								std::input_iterator_tag) {
				auto val = std::all_of(beg, end, p);
				if(val)
					ret = 1;
				else
					ret = 0;

			}
	};
	template<typename InputIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<InputIt>>
	bool all_of(InputIt beg, InputIt end, UnaryPredicate p) {
		Tpolicy Tp;
		bool ret = false;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return true;
		if(Tp.num_threads < 2)
			return std::all_of(beg, end, p);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(all_of_block<InputIt, UnaryPredicate>(), block_start,
					block_end, p, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		all_of_block<InputIt, UnaryPredicate>()(block_start, last, p,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());

		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		ret = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ret;
	}
	template<typename InputIt, typename UnaryPredicate>
	struct any_of_block {
			void operator ()(InputIt beg, InputIt end, UnaryPredicate p, int &ret,
								std::input_iterator_tag) {
				auto val = std::any_of(beg, end, p);
				if(val)
					ret = 1;
				else
					ret = 0;

			}
	};
	template<typename InputIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<InputIt>>
	bool any_of(InputIt beg, InputIt end, UnaryPredicate p) {
		Tpolicy Tp;
		bool ret = false;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return true;
		if(Tp.num_threads < 2)
			return std::any_of(beg, end, p);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(any_of_block<InputIt, UnaryPredicate>(), block_start,
					block_end, p, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		any_of_block<InputIt, UnaryPredicate>()(block_start, last, p,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());

		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		ret = std::any_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ret;
	}
	template<typename InputIt, typename UnaryPredicate>
	struct none_of_block {
			void operator ()(InputIt beg, InputIt end, UnaryPredicate p, int &ret,
								std::input_iterator_tag) {
				auto val = std::none_of(beg, end, p);
				if(val)
					ret = 1;
				else
					ret = 0;

			}
	};
	template<typename InputIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<InputIt>>
	bool none_of(InputIt beg, InputIt end, UnaryPredicate p) {
		Tpolicy Tp;
		bool ret = false;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return true;
		if(Tp.num_threads < 2)
			return std::none_of(beg, end, p);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(none_of_block<InputIt, UnaryPredicate>(), block_start,
					block_end, p, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
		}
		none_of_block<InputIt, UnaryPredicate>()(block_start, last, p,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());

		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		ret = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ret;
	}

	template<typename InputIt1, typename InputIt2>
	struct equal_block {
			bool operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, int &retval,
							std::input_iterator_tag) {
				auto ans = std::equal(beg1, end1, beg2);
				if(ans)
					retval = 1;
				else
					retval = 0;
				return ans;
			}
	};
	template<typename InputIt1, typename InputIt2>
	struct equal_block4 {
			bool operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, InputIt2 end2, int &retval,
							std::input_iterator_tag) {
				auto ans = std::equal(beg1, end1, beg2);
				if(ans)
					retval = 1;
				else
					retval = 0;
				return ans;
			}
	};

	template<typename InputIt, typename InputIt2, typename Tpolicy = LaunchPolicies<InputIt> >

	bool equal(InputIt beg1, InputIt end1, InputIt2 beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		// test to make sure they are at least as long
		{

			InputIt end2 = beg2
					+ static_cast<typename std::iterator_traits<InputIt>::difference_type>(Tp.length
							- 1);
			InputIt testend1 = beg1
					+ static_cast<typename std::iterator_traits<InputIt>::difference_type>(Tp.length
							- 1);
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
		if(Tp.num_threads < 2)
			return std::equal(beg1, end1, beg2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
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

		auto ans = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ans;
	}

	template<typename InputIt1, typename InputIt2, typename BinaryPredicate>
	struct equal_block2 {
			bool operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, BinaryPredicate p,
							int &retval, std::input_iterator_tag) {
				auto ans = std::equal(beg1, end1, beg2, p);
				if(ans)
					retval = 1;
				else
					retval = 0;
				return ans;
			}
	};

	template<typename InputIt, typename InputIt2, typename BinaryPredicate,
			typename Tpolicy = LaunchPolicies<InputIt> >

	bool equal(InputIt beg1, InputIt end1, InputIt2 beg2, BinaryPredicate p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		// test to make sure they are at least as long
		{

			InputIt end2 = beg2
					+ static_cast<typename std::iterator_traits<InputIt>::difference_type>(Tp.length
							- 1);
			InputIt testend1 = beg1
					+ static_cast<typename std::iterator_traits<InputIt>::difference_type>(Tp.length
							- 1);
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
		if(Tp.num_threads < 2)
			return std::equal(beg1, end1, beg2, p);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
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

		auto ans = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ans;
	}

	template<typename InputIt1, typename InputIt2, typename BinaryPredicate>
	struct equal_block3 {
			bool operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, InputIt2 end2,
							BinaryPredicate p, int &retval, std::input_iterator_tag) {
				auto ans = std::equal(beg1, end1, beg2, p);
				if(ans)
					retval = 1;
				else
					retval = 0;
				return ans;
			}
	};

	template<typename InputIt, typename InputIt2, typename BinaryPredicate,
			typename Tpolicy = LaunchPolicies<InputIt> >

	bool equal(InputIt beg1, InputIt end1, InputIt2 beg2, InputIt2 end2, BinaryPredicate p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(std::distance(beg1, end1) != std::distance(beg2, end2))
			return false;
		if(Tp.num_threads < 2)
			return std::equal(beg1, end1, end2, p);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt2 block_end2 = beg2;
		InputIt last = end1;
		InputIt2 last2 = end2;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			std::advance(block_end2, Tp.block_size);
			threads[i] = std::thread(equal_block3<InputIt, InputIt2, BinaryPredicate>(),
					block_start, block_end, block_start2, block_end2, p, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			block_start2 = block_end2;
		}
		equal_block3<InputIt, InputIt2, BinaryPredicate>()(block_start, last, block_start2, last2,
				p, std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ans;
	}

	//equal version 4
	template<typename InputIt, typename InputIt2, typename Tpolicy = LaunchPolicies<InputIt> >

	bool equal(InputIt beg1, InputIt end1, InputIt2 beg2, InputIt2 end2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		// test to make sure they are at least as long
		{

			InputIt end2 = beg2
					+ static_cast<typename std::iterator_traits<InputIt>::difference_type>(Tp.length
							- 1);
			InputIt testend1 = beg1
					+ static_cast<typename std::iterator_traits<InputIt>::difference_type>(Tp.length
							- 1);
			try {
				if(*end2 != *testend1)
					return false;
			}
			catch(...) {

				return false; //failed.
			}
			try {
				if((Tp.length == 0) && std::distance(beg2, end2)) {
					return false;
				}

			}
			catch(...) {
				// if both failed, then it means the both containers are empty return true
				return true;
			}
		}
		if(Tp.num_threads < 2)
			return std::equal(beg1, end1, beg2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt2 block_end2 = beg2;
		InputIt last = end1;
		InputIt2 last2 = end2;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			std::advance(block_end2, Tp.block_size);
			threads[i] = std::thread(equal_block4<InputIt, InputIt2>(), block_start, block_end,
					block_start2, block_end2, std::ref(output[i]),
					typename std::iterator_traits<InputIt>::iterator_category());
			block_start = block_end;
			block_start2 = block_end2;
		}
		equal_block4<InputIt, InputIt2>()(block_start, last, block_start2, last2,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<InputIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ans;
	}

	template<typename ForwardIt>
	struct max_element_block {
			void operator()(
					ForwardIt beg,
					ForwardIt end,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retval,
					std::forward_iterator_tag) {
				retval.first = (std::max_element(beg, end));
				retval.second = *(retval.first);
			}
	};

	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >

	ForwardIt max_element(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2)
			return std::max_element(beg, end);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(max_element_block<ForwardIt>(), block_start, block_end,
					std::ref(output[i]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			block_start = block_end;

		}
		max_element_block<ForwardIt>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans =
				std::max_element(output.begin(), output.end(),
						[](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
								std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return a.second<b.second;});
		return (*ans).first;
	}

	template<typename ForwardIt, typename Comp>
	struct max_element_block2 {
			void operator()(
					ForwardIt beg,
					ForwardIt end,
					Comp cmp,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retval,
					std::forward_iterator_tag) {
				retval.first = (std::max_element(beg, end, cmp));
				retval.second = *(retval.first);
			}
	};

	template<typename ForwardIt, typename Comp, typename Tpolicy = LaunchPolicies<ForwardIt> >

	ForwardIt max_element(ForwardIt beg, ForwardIt end, Comp cmp) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2)
			return std::max_element(beg, end, cmp);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(max_element_block2<ForwardIt, Comp>(), block_start, block_end,
					cmp, std::ref(output[i]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			block_start = block_end;

		}
		max_element_block2<ForwardIt, Comp>()(block_start, last, cmp,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans =
				std::max_element(output.begin(), output.end(),
						[&](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
								std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return cmp(a.second,b.second);});
		return (*ans).first;
	}
	// min_element
	template<typename ForwardIt>
	struct min_element_block {
			void operator()(
					ForwardIt beg,
					ForwardIt end,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retval,
					std::forward_iterator_tag) {
				retval.first = (std::min_element(beg, end));
				retval.second = *(retval.first);
			}
	};

	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >

	ForwardIt min_element(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2)
			return std::min_element(beg, end);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(min_element_block<ForwardIt>(), block_start, block_end,
					std::ref(output[i]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			block_start = block_end;

		}
		min_element_block<ForwardIt>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans =
				std::min_element(output.begin(), output.end(),
						[](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
								std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return a.second<b.second;});
		return (*ans).first;
	}

	template<typename ForwardIt, typename Comp>
	struct min_element_block2 {
			void operator()(
					ForwardIt beg,
					ForwardIt end,
					Comp cmp,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retval,
					std::forward_iterator_tag) {
				retval.first = (std::min_element(beg, end, cmp));
				retval.second = *(retval.first);
			}
	};

	template<typename ForwardIt, typename Comp, typename Tpolicy = LaunchPolicies<ForwardIt> >

	ForwardIt min_element(ForwardIt beg, ForwardIt end, Comp cmp) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2)
			return std::min_element(beg, end, cmp);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(min_element_block2<ForwardIt, Comp>(), block_start, block_end,
					cmp, std::ref(output[i]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			block_start = block_end;

		}
		min_element_block2<ForwardIt, Comp>()(block_start, last, cmp,
				std::ref(output[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans =
				std::min_element(output.begin(), output.end(),
						[&](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
								std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return cmp(a.second,b.second);});
		return (*ans).first;
	}

	//minmax_element
	template<typename ForwardIt>
	struct minmax_element_block {
			void operator()(
					ForwardIt beg,
					ForwardIt end,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retmin,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retmax,
					std::forward_iterator_tag) {
				auto ans = (std::minmax_element(beg, end));
				retmin.first = ans.first;
				retmin.second = *(ans.first);
				retmax.first = ans.second;
				retmax.second = *(ans.second);
			}
	};

	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >

	std::pair<ForwardIt, ForwardIt> minmax_element(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return std::pair<ForwardIt, ForwardIt>(end, end);
		if(Tp.num_threads < 2)
			return std::minmax_element(beg, end);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output1(
				Tp.num_threads);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output2(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(minmax_element_block<ForwardIt>(), block_start, block_end,
					std::ref(output1[i]), std::ref(output2[i]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			block_start = block_end;

		}
		minmax_element_block<ForwardIt>()(block_start, last, std::ref(output1[Tp.num_threads - 1]),
				std::ref(output2[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans1 =
				std::min_element(output1.begin(), output1.end(),
						[](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
								std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return a.second<b.second;});
		auto ans2 =
				std::max_element(output2.begin(), output2.end(),
						[](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
								std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return a.second<b.second;});
		return std::pair<ForwardIt, ForwardIt>((*ans1).first, (*ans2).first);

	}

	template<typename ForwardIt, typename Comp>
	struct minmax_element_block2 {
			void operator()(
					ForwardIt beg,
					ForwardIt end,
					Comp cmp,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retmin,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retmax,
					std::forward_iterator_tag) {
				auto ans = (std::minmax_element(beg, end, cmp));
				retmin.first = ans.first;
				retmin.second = *(ans.first);
				retmax.first = ans.second;
				retmax.second = *(ans.second);
			}
	};

	template<typename ForwardIt, typename Comp, typename Tpolicy = LaunchPolicies<ForwardIt> >

	std::pair<ForwardIt, ForwardIt> minmax_element(ForwardIt beg, ForwardIt end, Comp cmp) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return std::pair<ForwardIt, ForwardIt>(end, end);

		if(Tp.num_threads < 2)
			return std::minmax_element(beg, end);

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type>> output1(
				Tp.num_threads);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type>> output2(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;

		for(int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);

			threads[i] = std::thread(minmax_element_block2<ForwardIt, Comp>(), block_start,
					block_end, cmp, std::ref(output1[i]), std::ref(output2[i]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			block_start = block_end;

		}
		minmax_element_block2<ForwardIt, Comp>()(block_start, last, cmp,
				std::ref(output1[Tp.num_threads - 1]), std::ref(output2[Tp.num_threads - 1]),
				typename std::iterator_traits<ForwardIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		auto ans1 =
				std::min_element(output1.begin(), output1.end(),
						[&cmp](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
								std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return cmp(a.second,b.second);});
		auto ans2 =
				std::max_element(output2.begin(), output2.end(),
						[&cmp](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
								std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return cmp(a.second,b.second);});
		return std::pair<ForwardIt, ForwardIt>((*ans1).first, (*ans2).first);
	}

}

#endif /* PARLIB_H_ */
