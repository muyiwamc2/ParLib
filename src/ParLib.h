/*
 * ParLib.h
 *
 *  Created on: Nov 12, 2013
 *      Author: Olumuyiwa Oluwasanmi
 */

/*
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
#include <cmath>
#include <exception>
namespace parallel {
	enum class ThreadTypes {
			standard, async
	};
	/**
	 * @summary This class handles certain threading properties of each algorithm.
	 */
	template<typename InputIt, unsigned int blksz = 2048, unsigned int mxThread = 0,
			ThreadTypes tT = ThreadTypes::standard>
	class LaunchPolicies {
		public:
			ThreadTypes tTypes;
			unsigned long length;
			unsigned long hardware_threads = std::thread::hardware_concurrency();
			unsigned long max_hardware_threads = 0;
			unsigned long min_per_thread;
			unsigned long max_threads;
			unsigned long num_threads;
			unsigned long block_size;
			/**
			 * @summary sets the launch policies for the threads..
			 * @param beg Input iterator to the beginning of the input range
			 * @param end In put iterator to the end of the input range
			 */
			void SetLaunchPolicies(InputIt beg, InputIt end, unsigned long max_thread = mxThread,
								   unsigned long bsize = blksz) {
				block_size = bsize;
				tTypes = tT;
				length = std::distance(beg, end);
				hardware_threads = std::thread::hardware_concurrency();
				max_hardware_threads = max_thread;
				if(max_hardware_threads and max_hardware_threads > 0)
					hardware_threads = std::min(hardware_threads, max_hardware_threads);
				min_per_thread = block_size;
				max_threads = (length + min_per_thread) / min_per_thread;
				;
				num_threads = std::min(hardware_threads != 0 ? hardware_threads : 2, max_threads);
				block_size = length / num_threads;
			}
			/**
			 *
			 * @param dist parameter showing the length of the input container for the operation.
			 */
			void SetLaunchPolicies(typename std::iterator_traits<InputIt>::difference_type dist,
								   unsigned long max_thread = mxThread,
								   unsigned long bsize = blksz) {
				block_size = bsize;
				tTypes = tT;
				length = dist;
				hardware_threads = std::thread::hardware_concurrency();

				min_per_thread = block_size;
				max_threads = (length + min_per_thread) / min_per_thread;
				;
				num_threads = std::min(hardware_threads != 0 ? hardware_threads : 2, max_threads);
				block_size = length / num_threads;
			}
	};
	template<typename InputIt>
	class LaunchPolicies<InputIt, 2048, 0, ThreadTypes::standard> {
		public:
			ThreadTypes tTypes;
			unsigned long length;
			unsigned long hardware_threads;
			unsigned long min_per_thread;
			unsigned long max_threads;
			unsigned long max_hardware_threads = 0;
			unsigned long num_threads;
			unsigned long block_size;
			void SetLaunchPolicies(InputIt beg, InputIt end) {
				tTypes = ThreadTypes::standard;
				length = std::distance(beg, end);
				hardware_threads = std::thread::hardware_concurrency();
				if(max_hardware_threads and max_hardware_threads > 0)
					hardware_threads = std::min(max_hardware_threads, hardware_threads);
				min_per_thread = 2048;
				max_threads = (length + min_per_thread) / min_per_thread;
				;
				num_threads = std::min(hardware_threads != 0 ? hardware_threads : 2, max_threads);
				block_size = length / num_threads;
			}
			/**
			 *
			 * @param dist parameter showing the length of the input container for the operation.
			 */
			void SetLaunchPolicies(typename std::iterator_traits<InputIt>::difference_type dist) {
				tTypes = ThreadTypes::standard;
				length = dist;
				hardware_threads = std::thread::hardware_concurrency();
				min_per_thread = 2048;
				max_threads = (length + min_per_thread) / min_per_thread;
				;
				num_threads = std::min(hardware_threads != 0 ? hardware_threads : 2, max_threads);
				block_size = length / num_threads;
			}
	};

	/**
	 * handles each block of the foreach function.
	 */
	template<typename InputIt, typename UnaryFunction>
	struct foreach_block {
			/**
			 *
			 * @param beg InputIterator to the beginning of the input container
			 * @param end InputIterator to the end of the input container
			 * @param f Unary Function to be executed
			 * @param  used to make sure we at least have input iterators
			 */
			void operator ()(InputIt beg, InputIt end, UnaryFunction f, std::input_iterator_tag) {
				std::for_each(beg, end, f);
			}
	};
	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @param f Unary function to be executed for each element.
	 * @return
	 */
	template<typename InputIt, typename UnaryFunction, typename Tpolicy = LaunchPolicies<InputIt> >
	UnaryFunction for_each(InputIt beg, InputIt end, UnaryFunction f) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return f;
		if(Tp.num_threads < 2)
			return std::for_each(beg, end, f);
		std::vector<std::future<void>> futures(Tp.num_threads - 1);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		InputIt block_start = beg;
		InputIt block_end = beg;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(foreach_block<InputIt, UnaryFunction>(), block_start,
										 block_end, f, typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;
			}
			foreach_block<InputIt, UnaryFunction>()(block_start, end, f,
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, foreach_block<InputIt, UnaryFunction>(),
										block_start, block_end, f,
										typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;
			}
			foreach_block<InputIt, UnaryFunction>()(block_start, end, f,
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		return f;
	}
	/**
	 * Helper class for each block of the transform function
	 */
	template<typename InputIt, typename OutputIt, typename UnaryOperator>
	struct transform_block {
			/**
			 *
			 * @param first1 Input iterator to the beginning of the block.
			 * @param last1	Input iterator to the end of the block
			 * @param result Output Iterator : The beginning of the output iterator
			 * @param op	Operator which transforms elements in input container to output container.
			 * @param
			 */
			void operator()(InputIt first1, InputIt last1, OutputIt result, UnaryOperator op,
							std::input_iterator_tag) {
				std::transform(first1, last1, result, op);

			}
	};
	/**
	 * Helper class for each block of the transform function
	 */
	template<typename InputIt, typename InputIt2, typename OutputIt, typename BinaryOperator>
	struct transform_block2 {
			/**
			 *
			 * @param first1 Input iterator to the beginning of the block.
			 * @param last1	Input iterator to the end of the block
			 * @param first2 Input iterator to the beginning of the second input container
			 * @param result OutputIterator to the beginning of the output container.
			 * @param op The binary operator which transforms each element of the input containers to an element of the output container
			 * @param used to make sure we have the right input iterators
			 * @param used to make sure we have output iterators.
			 */
			void operator()(InputIt first1, InputIt last1, InputIt2 first2, OutputIt result,
							BinaryOperator op, std::input_iterator_tag, std::input_iterator_tag) {
				std::transform(first1, last1, first2, result, op);

			}
	};
	/**
	 *
	 * @param beg The beginning of the input container
	 * @param end The end of the input container
	 * @param result The beginning of the output iterator assumes that we have space end -beg in the output container
	 * @param op the operator to be applied.
	 * @return
	 */
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
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;
		OutputIt outblock_start = result;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(transform_block<InputIt, OutputIt, UnaryOperator>(),
										 block_start, block_end, outblock_start, op,
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(outblock_start, Tp.block_size);
			}
			transform_block<InputIt, OutputIt, UnaryOperator>()(block_start, last, outblock_start,
					op, typename std::iterator_traits<InputIt>::iterator_category());

			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										transform_block<InputIt, OutputIt, UnaryOperator>(), block_start, block_end,
										outblock_start, op,
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(outblock_start, Tp.block_size);
			}
			transform_block<InputIt, OutputIt, UnaryOperator>()(block_start, last, outblock_start,
					op, typename std::iterator_traits<InputIt>::iterator_category());

			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}
		return result;
	}

	/**
	 *
	 * @param beg1  Input iterator to the beginning of the first input container.
	 * @param end1	Input iterator to the end of the first container.
	 * @param beg2	Input iterator to the beginning of the second container.
	 * @param result Input iterator to the beginning of the output container. Assumes the output container is exactly as long as the input containers.
	 * @param op	Binary operator on the two input elements to give an output element.
	 * @return
	 */
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
			std::vector<std::future<void>> futures(Tp.num_threads - 1);

			InputIt block_start1 = beg1;
			InputIt2 block_start2 = beg2;
			InputIt block_end1 = beg1;
			InputIt last1 = end1;

			OutputIt outblock_start = result;
			if(Tp.tTypes == ThreadTypes::standard) {
				for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

					std::advance(block_end1, Tp.block_size);
					threads[i] = std::thread(
							transform_block2<InputIt, InputIt2, OutputIt, BinaryOperator>(),
							block_start1, block_end1, block_start2, outblock_start, op,
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
			}

			if(Tp.tTypes == ThreadTypes::async) {
				for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

					std::advance(block_end1, Tp.block_size);
					futures[i] = std::async(std::launch::async,
											transform_block2<InputIt, InputIt2, OutputIt, BinaryOperator>(),
											block_start1, block_end1, block_start2, outblock_start, op,
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
				std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
			}
			return result;
	}
	/**
	 * Helper for the fill function
	 */
	template<typename ForwardIt, typename T>
	struct fill_block {
			/**
			 *
			 * @param beg the beginning of the input block
			 * @param end  the end of the input block
			 * @param value to be filled.
			 * @param makes sure we have a forward iterator
			 */
			void operator()(ForwardIt beg, ForwardIt end, const T& value,
							std::forward_iterator_tag) {
				std::fill(beg, end, value);
			}
	};
	/**
	 * helper for fill_n function.
	 */
	template<typename OutputIt, typename Size, typename T>
	struct fill_n_block {
			/**
			 *
			 * @param beg The beginning of the input block
			 * @param end The end of the input block.
			 * @param count The number of elements to fill in this block.
			 * @param value The value to be filled
			 * @param ret The value to return to use later.
			 */
			void operator()(OutputIt beg, OutputIt end, Size count, const T& value,
							std::pair<OutputIt, bool> &ret) {
				count = static_cast<Size>(std::distance(beg, end));
				std::fill_n(beg, count, value);
				ret.first = beg;
				ret.second = true;
			}

	};
	template<typename OutputIt, typename Size, typename T>
	struct fill_n_block2 {
			/**
			 *
			 * @param beg The beginning of the input block
			 * @param end The end of the input block.
			 * @param count The number of values to fill.
			 * @param value The value to be filled
			 * @param ret The value to be returned to the rest
			 * @return
			 */
			OutputIt operator()(OutputIt beg, OutputIt end, Size count, const T& value,
								std::pair<OutputIt, bool> &ret) {
				count = static_cast<Size>(std::distance(beg, end));
				ret.first = std::fill_n(beg, count, value);
				ret.second = true;
				return ret.first;
			}

	};
	/**
	 *@summary fills the whole range with value
	 * @param beg  Input iterator to the beginning of the input range
	 * @param end  Input iterator to the end of the input range
	 * @param value The value to be filled.
	 */
	template<typename InputIt, typename T, typename Tpolicy = LaunchPolicies<InputIt> >
	void fill(InputIt beg, InputIt end, T &value) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return;
		if(Tp.num_threads < 2)
			return std::fill(beg, end, value);

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		InputIt block_start = beg;
		InputIt block_end = beg;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(fill_block<InputIt, T>(), block_start, block_end,
										 std::ref(value),
										 typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;
			}
			fill_block<InputIt, T>()(block_start, end, std::ref(value),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, fill_block<InputIt, T>(), block_start,
										block_end, std::ref(value),
										typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;
			}
			fill_block<InputIt, T>()(block_start, end, std::ref(value),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

	}
	/**
	 *@summary old function likely deprecated
	 * @param beg Output iterator to the beginning of the input range
	 * @param count number of values to be filled
	 * @param value the value to be filled.
	 */
	template<typename OutputIt, typename Size, typename T, typename Tpolicy = LaunchPolicies<
			OutputIt> >
	void fill_n_old(OutputIt beg, Size count, T &value) {
		Tpolicy Tp;
		OutputIt end = beg;
		std::advance(end, count);
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return;
		if(Tp.num_threads < 2) {
			std::fill_n(beg, count, std::ref(value));
			return;
		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::pair<OutputIt, bool>> output(Tp.num_threads);
		OutputIt block_start = beg;
		OutputIt block_end = beg;

		for(unsigned int i; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(fill_n_block<OutputIt, Size, T>(), block_start, block_end,
									 count, std::ref(value), std::ref(output[i]));

			block_start = block_end;
		}
		fill_n_block<OutputIt, Size, T>()(block_start, end, count, std::ref(value),
				std::ref(output[Tp.num_threads - 1]));
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

	}
	/**
	 * @summary fill count values starting from beg
	 * @param beg Output iterator to the beginning of the input range
	 * @param count number of values to be filled.
	 * @param value The value to fill
	 * @return Output iterator to the last value filled.
	 */
	template<typename OutputIt, typename Size, typename T, typename Tpolicy = LaunchPolicies<
			OutputIt> >
	OutputIt fill_n(OutputIt beg, Size count, T &value) {
		Tpolicy Tp;
		OutputIt end = beg;
		std::advance(end, count);
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2) {
			return std::fill_n(beg, count, std::ref(value));

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<OutputIt> > futures(Tp.num_threads - 1);
		std::vector<std::pair<OutputIt, bool>> output(Tp.num_threads);

		OutputIt block_start = beg;
		OutputIt block_end = beg;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(fill_n_block2<OutputIt, Size, T>(), block_start, block_end,
										 count, std::ref(value), std::ref(output[i]));

				block_start = block_end;
			}
			fill_n_block2<OutputIt, Size, T>()(block_start, end, count, std::ref(value),
					std::ref(output[Tp.num_threads - 1]));
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, fill_n_block2<OutputIt, Size, T>(),
										block_start, block_end, count, std::ref(value), std::ref(output[i]));

				block_start = block_end;
			}
			fill_n_block2<OutputIt, Size, T>()(block_start, end, count, std::ref(value),
					std::ref(output[Tp.num_threads - 1]));
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future < OutputIt > ::wait));
		}
		auto ans = std::find_if(output.rbegin(), output.rend(),
								[](std::pair<OutputIt, bool> v)->bool {return v.second;});

		return ans->first;

	}

	/**
	 * Helper class for generate function
	 */
	template<typename ForwardIt, typename Generator>
	struct generate_block {
			/**
			 *
			 * @param beg iterator to the beginning of the block for the input container.
			 * @param end iterator to the end of the block for the input container
			 * @param g Generator function used in *ForwardIt = g()
			 * @param make sure that the iterator is a forward iterator
			 */
			void operator()(ForwardIt beg, ForwardIt end, Generator g, std::forward_iterator_tag) {
				std::generate(beg, end, g);
			}
	};

	/**
	 * Helper class for generate_n function
	 */
	template<typename OutputIt, typename Size, typename Generator>
	struct generate_n_block {
			/**
			 *
			 * @param beg beginning iterator to the input container
			 * @param cnt counter for the block , how many elements to generate for.
			 * @param g Generator function used in *ForwardIt = g()

			 */
			void operator()(OutputIt beg, Size cnt, Generator g, std::pair<OutputIt, bool> &ret) {

				ret.first = std::generate_n(beg, cnt, g);
				ret.second = true;
			}
	};

	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @param g function to be executed to fill every element.
	 * @return
	 */
	template<typename ForwardIt, typename Generator, typename Tpolicy = LaunchPolicies<ForwardIt> >
	void generate(ForwardIt beg, ForwardIt end, Generator g) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return;
		if(Tp.num_threads < 2) {
			std::generate(beg, end, g);
			return;
		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::future<void>> futures(Tp.num_threads - 1);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(generate_block<ForwardIt, Generator>(), block_start,
										 block_end, g,
										 typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			generate_block<ForwardIt, Generator>()(block_start, end, g,
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, generate_block<ForwardIt, Generator>(),
										block_start, block_end, g,
										typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			generate_block<ForwardIt, Generator>()(block_start, end, g,
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

	}

	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param count how many elements we have to generate for.
	 * @param g function to be executed to fill every element.
	 * @return Output iterator to one after the last value changed.
	 */
	template<typename OutputIt, typename Size, typename Generator,
	typename Tpolicy = LaunchPolicies<OutputIt> >
	OutputIt generate_n(OutputIt beg, Size count, Generator g) {
		OutputIt end = beg;
		std::advance(end, count);
		if(count <= 0)
			return beg;
		Tpolicy Tp;

		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2)
			return std::generate_n(beg, count, g);

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::future<void>> futures(Tp.num_threads - 1);
		std::vector<std::pair<OutputIt, bool>> output(Tp.num_threads);
		OutputIt block_start = beg;
		OutputIt block_end = beg;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				Size cnt = static_cast<Size>(std::distance(block_start, block_end));
				threads[i] = std::thread(generate_n_block<OutputIt, Size, Generator>(), block_start,
										 cnt, g, std::ref(output[i]));

				block_start = block_end;
			}
			Size cnt = static_cast<Size>(std::distance(block_start, end));
			generate_n_block<OutputIt, Size, Generator>()(block_start, cnt, g,
					std::ref(output[Tp.num_threads - 1]));
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				Size cnt = static_cast<Size>(std::distance(block_start, block_end));
				futures[i] = std::async(std::launch::async,
										generate_n_block<OutputIt, Size, Generator>(), block_start, cnt, g,
										std::ref(output[i]));

				block_start = block_end;
			}
			Size cnt = static_cast<Size>(std::distance(block_start, end));
			generate_n_block<OutputIt, Size, Generator>()(block_start, cnt, g,
					std::ref(output[Tp.num_threads - 1]));
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}
		return (output[Tp.num_threads - 1]).first;

	}
	template<typename InputIt, typename T>
	struct accumulate_block {
			/**
			 *
			 * @param beg Input iterator to the beginning of the input block
			 * @param end Input iterator to the end of the input block
			 * @param ret The value to be returned from this block
			 * @param To make sure we have input iterators
			 * @return the sum in this block.
			 */
			T operator()(InputIt beg, InputIt end, T &ret, std::input_iterator_tag) {

				ret = std::accumulate(beg, end, ret);
				return ret;

			}
	};

	template<typename InputIt, typename T, typename BinaryOperator>
	struct accumulate_block2 {
			/**
			 *
			 * @param beg Input iterator to the beginning of the input block
			 * @param end Input iterator to the end of the input block
			 * @param ret The value to be returned from this block
			 * @param op Binary operator used for accumulate
			 * @param To make sure we are using an input iterator
			 * @return returned accumulate for this block.
			 */
			T operator()(InputIt beg, InputIt end, T &ret, BinaryOperator op,
						 std::input_iterator_tag) {

				ret = std::accumulate(beg, end, ret, op);
				return ret;

			}
	};
	/**
	 *@summary performs an accumulation or reduce or foldl operation on an input rage. Can perform foldr by reversing the input range.
	 * @param beg Input iterator to the beginning of the input range
	 * @param end Input iterator to the end of the input range
	 * @param init The initial value
	 * @return The result of the reduce or foldl operation.
	 */
	template<typename InputIt, typename T, typename Tpolicy = LaunchPolicies<InputIt>>
			T accumulate(InputIt beg, InputIt end, T init) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return init;
		if(Tp.num_threads < 2)
			return std::accumulate(beg, end, init);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<T> > futures(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(accumulate_block<InputIt, T>(), block_start, block_end,
										 std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			accumulate_block<InputIt, T>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, accumulate_block<InputIt, T>(),
										block_start, block_end, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			accumulate_block<InputIt, T>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future < T > ::wait));
		}
		return std::accumulate(output.begin(), output.end(), init);
	}

	/**
	 *
	 *@summary performs an accumulation or reduce or foldl operation on an input rage. Can perform foldr by reversing the input range.
	 * @param beg Input iterator to the beginning of the input range
	 * @param end Input iterator to the end of the input range
	 * @param init The initial value
	 * @param op binary operator used in the reduce
	 * @return result of the accumulate/foldl/reduce operation.
	 */
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
		std::vector < std::future<T> > futures(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(accumulate_block2<InputIt, T, BinaryOperator>(),
										 block_start, block_end, std::ref(output[i]), op,
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			accumulate_block2<InputIt, T, BinaryOperator>()(block_start, last,
					std::ref(output[Tp.num_threads - 1]), op,
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										accumulate_block2<InputIt, T, BinaryOperator>(), block_start, block_end,
										std::ref(output[i]), op,
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			accumulate_block2<InputIt, T, BinaryOperator>()(block_start, last,
					std::ref(output[Tp.num_threads - 1]), op,
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future < T > ::wait));
		}

		return std::accumulate(output.begin(), output.end(), init, op);
	}

	template<typename InputIt, typename T, typename UnaryPred>
	struct accumulate_if_block {
			/**
			 *
			 * @param beg
			 * @param end
			 * @param ret
			 * @param p
			 * @param
			 * @return
			 */
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
			/**
			 *
			 * @param beg
			 * @param end
			 * @param ret
			 * @param p
			 * @param op
			 * @param
			 * @return
			 */
			T operator()(InputIt beg, InputIt end, T& ret, UnaryPred p, BinaryOperator op,
						 std::input_iterator_tag) {
				for(; beg != end; ++beg) {
					if(p(*beg))
						ret = op(ret, *beg);
				}
				return ret;
			}
	};
	/**
	 *
	 * @param beg
	 * @param end
	 * @param init
	 * @param p
	 * @return
	 */
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
		std::vector < std::future<T> > futures(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										accumulate_if_block<InputIt, T, UnaryPred>(), block_start, block_end,
										std::ref(output[i]), p,
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			accumulate_if_block<InputIt, T, UnaryPred>()(block_start, last,
					std::ref(output[Tp.num_threads - 1]), p,
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future < T > ::wait));
		}
		return std::accumulate(output.begin(), output.end(), init);
	}
	/**
	 *
	 * @param beg
	 * @param end
	 * @param init
	 * @param p
	 * @param op
	 * @return
	 */
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
		std::vector < std::future<T> > futures(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(
						accumulate_if_block2<InputIt, T, UnaryPred, BinaryOperator>(), block_start,
						block_end, std::ref(output[i]), p, op,
						typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			accumulate_if_block2<InputIt, T, UnaryPred, BinaryOperator>()(block_start, last,
					std::ref(output[Tp.num_threads - 1]), p, op,
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										accumulate_if_block2<InputIt, T, UnaryPred, BinaryOperator>(), block_start,
										block_end, std::ref(output[i]), p, op,
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			accumulate_if_block2<InputIt, T, UnaryPred, BinaryOperator>()(block_start, last,
					std::ref(output[Tp.num_threads - 1]), p, op,
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future < T > ::wait));
		}

		return std::accumulate(output.begin(), output.end(), init, op);
	}
	/**
	 *
	 */
	template<typename InputIt1, typename InputIt2, typename T>
	struct inner_product_block {
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param retval
			 * @param
			 * @param
			 */
			void operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, T &retval,
							std::input_iterator_tag, std::input_iterator_tag) {
				retval = std::inner_product(beg1, end1, beg2, retval);

			}
	};
	/**
	 *
	 */
	template<typename InputIt1, typename InputIt2, typename T, typename BinaryOp1,
	typename BinaryOp2>
	struct inner_product_block2 {
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param retval
			 * @param op1
			 * @param op2
			 * @param
			 * @param
			 */
			void operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, T &retval, BinaryOp1 op1,
							BinaryOp2 op2, std::input_iterator_tag, std::input_iterator_tag) {
				retval = std::inner_product(beg1, end1, beg2, retval, op1, op2);

			}
	};
	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param init
	 * @return
	 */
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
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt last = end1;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										inner_product_block<InputIt, InputIt2, T>(), block_start, block_end,
										block_start2, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category(),
										typename std::iterator_traits<InputIt2>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}
			inner_product_block<InputIt, InputIt2, T>()(block_start, last, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category(),
					typename std::iterator_traits<InputIt2>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		return std::accumulate(output.begin(), output.end(), init);
	}
	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param init
	 * @param op1
	 * @param op2
	 * @return
	 */
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
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<T> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt last = end1;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(
						inner_product_block2<InputIt, InputIt2, T, BinaryOp1, BinaryOp2>(),
						block_start, block_end, block_start2, std::ref(output[i]), op1, op2,
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
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										inner_product_block2<InputIt, InputIt2, T, BinaryOp1, BinaryOp2>(),
										block_start, block_end, block_start2, std::ref(output[i]), op1, op2,
										typename std::iterator_traits<InputIt>::iterator_category(),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}
			inner_product_block2<InputIt, InputIt2, T, BinaryOp1, BinaryOp2>()(block_start, last,
					block_start2, std::ref(output[Tp.num_threads - 1]), op1, op2,
					typename std::iterator_traits<InputIt>::iterator_category(),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

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
	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @return
	 */
	template<typename InputIt, typename OutputIt, typename Tpolicy = LaunchPolicies<InputIt>>
			OutputIt adjacent_difference(InputIt beg1, InputIt end1, OutputIt beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2)
			return std::adjacent_difference(beg1, end1, beg2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<OutputIt> output(Tp.num_threads);
		typedef typename std::iterator_traits<InputIt>::value_type T1;
		typedef typename std::iterator_traits<OutputIt>::value_type T2;
		std::vector<std::pair<T1, T2>> initVals(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		OutputIt block_start2 = beg2;
		InputIt last = end1;

		//initialise values and reset iterators after all is done.
		for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {
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
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(adjacent_difference_block<InputIt, OutputIt>(),
										 block_start, block_end, block_start2, beg1, initVals[i].first,
										 initVals[i].second, std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}

			adjacent_difference_block<InputIt, OutputIt>()(block_start, last, block_start2, beg1,
					initVals[Tp.num_threads - 1].first, initVals[Tp.num_threads - 1].second,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async,
										adjacent_difference_block<InputIt, OutputIt>(), block_start, block_end,
										block_start2, beg1, initVals[i].first, initVals[i].second,
										std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}

			adjacent_difference_block<InputIt, OutputIt>()(block_start, last, block_start2, beg1,
					initVals[Tp.num_threads - 1].first, initVals[Tp.num_threads - 1].second,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

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
	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param op
	 * @return
	 */
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
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<OutputIt> output(Tp.num_threads);
		typedef typename std::iterator_traits<InputIt>::value_type T1;
		typedef typename std::iterator_traits<OutputIt>::value_type T2;
		std::vector<std::pair<T1, T2>> initVals(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		OutputIt block_start2 = beg2;
		InputIt last = end1;

		//initialise values and reset iterators after all is done.
		for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {
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
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(adjacent_difference_block2<InputIt, OutputIt, BinaryOp>(),
										 block_start, block_end, block_start2, op, beg1, initVals[i].first,
										 initVals[i].second, std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}

			adjacent_difference_block2<InputIt, OutputIt, BinaryOp>()(block_start, last,
					block_start2, op, beg1, initVals[Tp.num_threads - 1].first,
					initVals[Tp.num_threads - 1].second, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async,
										adjacent_difference_block2<InputIt, OutputIt, BinaryOp>(), block_start,
										block_end, block_start2, op, beg1, initVals[i].first, initVals[i].second,
										std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}

			adjacent_difference_block2<InputIt, OutputIt, BinaryOp>()(block_start, last,
					block_start2, op, beg1, initVals[Tp.num_threads - 1].first,
					initVals[Tp.num_threads - 1].second, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

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
			 * @param make sure we have an input iterator.
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
	/**
	 * @summary returns the partial sums in the output countainer starting at beg2
	 * @param beg1 input iterator to the beginning of the input range
	 * @param end1 input iterator to the end of the input range
	 * @param beg2 output iterator to the beginning of the output range
	 * @return Output iterator to the last item
	 */
	template<typename InputIt, typename OutputIt, typename Tpolicy = LaunchPolicies<InputIt>>
			OutputIt partial_sum(InputIt beg1, InputIt end1, OutputIt beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2)
			return std::partial_sum(beg1, end1, beg2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<OutputIt> output(Tp.num_threads);
		std::vector<typename std::iterator_traits<InputIt>::value_type> parsum(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		OutputIt block_start2 = beg2;
		InputIt last = end1;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(partial_sum_block<InputIt, OutputIt>(), block_start,
										 block_end, block_start2, std::ref(parsum[i]), std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}

			partial_sum_block<InputIt, OutputIt>()(block_start, last, block_start2,
					std::ref(parsum[Tp.num_threads - 1]), std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async, partial_sum_block<InputIt, OutputIt>(),
										block_start, block_end, block_start2, std::ref(parsum[i]),
										std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}

			partial_sum_block<InputIt, OutputIt>()(block_start, last, block_start2,
					std::ref(parsum[Tp.num_threads - 1]), std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

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
		for(unsigned int i = 1; i < (Tp.num_threads - 1); i++) {

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
	/**
	 *@summary computes the partial sums of elements of length end - beg,
	 *			the output container must also be end -beg long.
	 * @param beg1 Input iterator to the beginning of the input range
	 * @param end1 Input iterator to the end of the input range
	 * @param beg2 Output iterator to the beginning of the output container
	 * @param op Binary operator for the partial sum
	 * @return Output iterator to the last element of the partial sum
	 */
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
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<OutputIt> output(Tp.num_threads);
		std::vector<typename std::iterator_traits<InputIt>::value_type> parsum(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		OutputIt block_start2 = beg2;
		InputIt last = end1;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(partial_sum_block2<InputIt, OutputIt, BinaryOp>(),
										 block_start, block_end, block_start2, op, std::ref(parsum[i]),
										 std::ref(output[i]),
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
			for(unsigned int i = 1; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(partial_sum_block2<InputIt, OutputIt, BinaryOp>(),
										 block_start, block_end, block_start2, op, std::ref(parsum[i - 1]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}

			partial_sum_block2<InputIt, OutputIt, BinaryOp>()(block_start, last, block_start2, op,
					std::ref(parsum[Tp.num_threads - 2]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin() + 1, threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async,
										partial_sum_block2<InputIt, OutputIt, BinaryOp>(), block_start, block_end,
										block_start2, op, std::ref(parsum[i]), std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}

			partial_sum_block2<InputIt, OutputIt, BinaryOp>()(block_start, last, block_start2, op,
					std::ref(parsum[Tp.num_threads - 1]), std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));

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
			for(unsigned int i = 1; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async,
										partial_sum_block2<InputIt, OutputIt, BinaryOp>(), block_start, block_end,
										block_start2, op, std::ref(parsum[i - 1]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}

			partial_sum_block2<InputIt, OutputIt, BinaryOp>()(block_start, last, block_start2, op,
					std::ref(parsum[Tp.num_threads - 2]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin() + 1, futures.end(),
						  std::mem_fn(&std::future<void>::wait));
		}

		return output[Tp.num_threads - 1];
	}

	template<typename InputIt, typename T>
	struct find_block {
			/**
			 *
			 * @param beg
			 * @param end
			 * @param vl
			 * @param ret
			 * @param
			 */
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
	/**
	 *
	 */
	template<typename ForwardIt>
	struct adjacent_find_block {
			/**
			 *
			 * @param beg
			 * @param end
			 * @param ret
			 * @param
			 */
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
	/**
	 *
	 * @param beg
	 * @param end
	 * @param val
	 * @return
	 */
	template<typename InputIt, typename T, typename Tpolicy = LaunchPolicies<InputIt> >
	InputIt find(InputIt beg, InputIt end, const T & val) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return end;
		if(Tp.num_threads < 2)
			return std::find(beg, end, val);

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<InputIt, bool>> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(find_block<InputIt, T>(), block_start, block_end, val,
										 std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			find_block<InputIt, T>()(block_start, last, val, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, find_block<InputIt, T>(), block_start,
										block_end, val, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			find_block<InputIt, T>()(block_start, last, val, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));

		}
		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<InputIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}
	/**
	 *
	 * @param beg
	 * @param end
	 * @return
	 */
	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >
	ForwardIt adjacent_find(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return end;
		if(Tp.num_threads < 2)
			return std::adjacent_find(beg, end);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::future<void>> futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, bool>> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		ForwardIt last = end;
		ForwardIt block_end2 = block_end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				block_end2++;
				threads[i] = std::thread(adjacent_find_block<ForwardIt>(), block_start, block_end2,
										 std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			adjacent_find_block<ForwardIt>()(block_start, last,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				block_end2++;
				futures[i] = std::async(std::launch::async, adjacent_find_block<ForwardIt>(),
										block_start, block_end2, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			adjacent_find_block<ForwardIt>()(block_start, last,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}

	template<typename InputIt, typename UnaryPredicate>
	struct find_if_block {
			/**
			 *
			 * @param beg
			 * @param end
			 * @param p
			 * @param ret
			 * @param
			 */
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
			/**
			 *
			 * @param beg
			 * @param end
			 * @param p
			 * @param ret
			 * @param
			 */
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
	/**
	 *
	 * @param beg
	 * @param end
	 * @param p
	 * @return
	 */
	template<typename InputIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<InputIt> >
	InputIt find_if(InputIt beg, InputIt end, UnaryPredicate p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return end;
		if(Tp.num_threads < 2)
			return std::find_if(beg, end, p);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::future<void>> futures(Tp.num_threads - 1);
		std::vector<std::pair<InputIt, bool>> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										find_if_block<InputIt, UnaryPredicate>(), block_start, block_end, p,
										std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			find_if_block<InputIt, UnaryPredicate>()(block_start, last, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<InputIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}
	/**
	 *
	 * @param beg
	 * @param end
	 * @param p
	 * @return
	 */
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
		std::vector<std::future<void>> futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, bool>> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		ForwardIt block_end2 = beg;
		ForwardIt last = end;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				block_end2++;
				futures[i] = std::async(std::launch::async,
										adjacent_find2_block<ForwardIt, BinaryPredicate>(), block_start, block_end2,
										p, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			adjacent_find2_block<ForwardIt, BinaryPredicate>()(block_start, last, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}
		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}
	/**
	 *
	 * @param beg
	 * @param end
	 * @param p
	 * @return
	 */
	template<typename InputIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<InputIt>>
			InputIt find_if_not(InputIt beg, InputIt end, UnaryPredicate p) {
		return parallel::find_if<InputIt, UnaryPredicate, Tpolicy>(beg, end, std::not1(p));
	}

	template<typename ForwardIt1, typename ForwardIt2>
	struct find_first_of_block {
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param end2
			 * @param retval
			 * @param
			 * @param
			 * @return
			 */
			ForwardIt1 operator()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
								  ForwardIt2 end2, std::pair<ForwardIt1, bool>&retval,
								  std::forward_iterator_tag, std::forward_iterator_tag) {
				retval.first = std::find_first_of(beg1, end1, beg2, end2);
				if(retval.first == end1)
					retval.second = false;
				else
					retval.second = true;
				return retval.first;
			}
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param end2
			 * @param retval
			 * @param
			 * @param
			 * @return
			 */
			ForwardIt1 operator()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
								  ForwardIt2 end2, std::pair<ForwardIt1, bool>&retval,
								  std::input_iterator_tag, std::input_iterator_tag) {
				retval.first = std::find_first_of(beg1, end1, beg2, end2);
				if(retval.first == end1)
					retval.second = false;
				else
					retval.second = true;
				return retval.first;
			}
	};
	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param end2
	 * @return
	 */
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
		std::vector < std::future < ForwardIt1 >> futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 last = end1;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(find_first_of_block<ForwardIt1, ForwardIt2>(), block_start,
										 block_end, beg2, end2, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt1>::iterator_category(),
										 typename std::iterator_traits<ForwardIt1>::iterator_category());
				block_start = block_end;
			}
			find_first_of_block<ForwardIt1, ForwardIt2>()(block_start, last, beg2, end2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt1>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										find_first_of_block<ForwardIt1, ForwardIt2>(), block_start, block_end, beg2,
										end2, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt1>::iterator_category(),
										typename std::iterator_traits<ForwardIt1>::iterator_category());
				block_start = block_end;
			}
			find_first_of_block<ForwardIt1, ForwardIt2>()(block_start, last, beg2, end2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt1>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future < ForwardIt1 > ::wait));
		}

		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end1;
		else
			return ans->first;
	}

	template<typename ForwardIt1, typename ForwardIt2, typename BinaryOp>
	struct find_first_of_block2 {
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param end2
			 * @param op
			 * @param retval
			 * @param
			 * @param
			 * @return
			 */
			ForwardIt1 operator()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
								  ForwardIt2 end2, BinaryOp op,
								  std::pair<ForwardIt1, bool>&retval, std::forward_iterator_tag,
								  std::forward_iterator_tag) {
				retval.first = std::find_first_of(beg1, end1, beg2, end2, op);
				if(retval.first == end1)
					retval.second = false;
				else
					retval.second = true;
				return retval.first;
			}
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param end2
			 * @param op
			 * @param retval
			 * @param
			 * @param
			 * @return
			 */
			ForwardIt1 operator()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
								  ForwardIt2 end2, BinaryOp op,
								  std::pair<ForwardIt1, bool>&retval, std::input_iterator_tag,
								  std::input_iterator_tag) {
				retval.first = std::find_first_of(beg1, end1, beg2, end2, op);
				if(retval.first == end1)
					retval.second = false;
				else
					retval.second = true;
				return retval.first;
			}
	};

	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param end2
	 * @param op
	 * @return
	 */
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
		std::vector < std::future<ForwardIt1> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 last = end1;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(find_first_of_block2<ForwardIt1, ForwardIt2, BinaryOp>(),
										 block_start, block_end, beg2, end2, op, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt1>::iterator_category(),
										 typename std::iterator_traits<ForwardIt1>::iterator_category());
				block_start = block_end;
			}
			find_first_of_block2<ForwardIt1, ForwardIt2, BinaryOp>()(block_start, last, beg2, end2,
					op, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt1>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										find_first_of_block2<ForwardIt1, ForwardIt2, BinaryOp>(), block_start,
										block_end, beg2, end2, op, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt1>::iterator_category(),
										typename std::iterator_traits<ForwardIt1>::iterator_category());
				block_start = block_end;
			}
			find_first_of_block2<ForwardIt1, ForwardIt2, BinaryOp>()(block_start, last, beg2, end2,
					op, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt1>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future < ForwardIt1 > ::wait));
		}

		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end1;
		else
			return ans->first;
	}
	template<typename ForwardIt1, typename ForwardIt2>
	struct search_block {
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param end2
			 * @param ret
			 * @param
			 * @param
			 * @return
			 */
			ForwardIt1 operator ()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
								   ForwardIt2 end2, std::pair<ForwardIt1, bool> &ret,
								   std::forward_iterator_tag, std::forward_iterator_tag) {

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
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param end2
			 * @param p
			 * @param ret
			 * @param
			 * @param
			 * @return
			 */
			ForwardIt1 operator ()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
								   ForwardIt2 end2, BinaryPredicate p,
								   std::pair<ForwardIt1, bool> &ret, std::forward_iterator_tag,
								   std::forward_iterator_tag) {

				ret.first = std::search(beg1, end1, beg2, end2, p);
				if(ret.first == end1)
					ret.second = false;
				else
					ret.second = true;
				return ret.first;
			}
	};

	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param end2
	 * @return
	 */
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
		std::vector < std::future<ForwardIt1> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 block_end2 = beg1;
		ForwardIt1 last = end1;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, std::distance(beg2, end2));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				threads[i] = std::thread(search_block<ForwardIt1, ForwardIt2>(), block_start,
										 block_end2, beg2, end2, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt1>::iterator_category(),
										 typename std::iterator_traits<ForwardIt2>::iterator_category());
				block_start = block_end;
			}
			search_block<ForwardIt1, ForwardIt2>()(block_start, last, beg2, end2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt2>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, std::distance(beg2, end2));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				futures[i] = std::async(std::launch::async, search_block<ForwardIt1, ForwardIt2>(),
										block_start, block_end2, beg2, end2, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt1>::iterator_category(),
										typename std::iterator_traits<ForwardIt2>::iterator_category());
				block_start = block_end;
			}
			search_block<ForwardIt1, ForwardIt2>()(block_start, last, beg2, end2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt2>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future < ForwardIt1 > ::wait));
		}

		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end1;
		else
			return ans->first;
	}
	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param end2
	 * @param p
	 * @return
	 */
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
		std::vector < std::future<ForwardIt1> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 block_end2 = beg1;
		ForwardIt1 last = end1;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, std::distance(beg2, end2));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				threads[i] = std::thread(search_block2<ForwardIt1, ForwardIt2, BinaryPredicate>(),
										 block_start, block_end2, beg2, end2, p, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt1>::iterator_category(),
										 typename std::iterator_traits<ForwardIt2>::iterator_category());
				block_start = block_end;
			}
			search_block2<ForwardIt1, ForwardIt2, BinaryPredicate>()(block_start, last, beg2, end2,
					p, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt2>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, std::distance(beg2, end2));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				futures[i] = std::async(std::launch::async,
										search_block2<ForwardIt1, ForwardIt2, BinaryPredicate>(), block_start,
										block_end2, beg2, end2, p, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt1>::iterator_category(),
										typename std::iterator_traits<ForwardIt2>::iterator_category());
				block_start = block_end;
			}
			search_block2<ForwardIt1, ForwardIt2, BinaryPredicate>()(block_start, last, beg2, end2,
					p, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt2>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future < ForwardIt1 > ::wait));
		}

		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end1;
		else
			return ans->first;
	}
	/**
	 *
	 */
	template<typename ForwardIt, typename Size, typename T>
	struct search_n_block {
			/**
			 *
			 * @param beg
			 * @param end
			 * @param count
			 * @param value
			 * @param ret
			 * @param
			 * @return
			 */
			ForwardIt operator ()(ForwardIt beg, ForwardIt end, Size count, const T& value,
								  std::pair<ForwardIt, bool> &ret, std::forward_iterator_tag) {

				ret.first = std::search_n(beg, end, count, value);
				if(ret.first == end)
					ret.second = false;
				else
					ret.second = true;
				return ret.first;
			}
	};
	template<typename ForwardIt, typename Size, typename T, typename BinaryPredicate>
	struct search_n_block2 {
			/**
			 *
			 * @param beg
			 * @param end
			 * @param count
			 * @param value
			 * @param p
			 * @param ret
			 * @param
			 * @return
			 */
			ForwardIt operator ()(ForwardIt beg, ForwardIt end, Size count, const T& value,
								  BinaryPredicate p, std::pair<ForwardIt, bool> &ret,
								  std::forward_iterator_tag) {

				ret.first = std::search_n(beg, end, count, value, p);
				if(ret.first == end)
					ret.second = false;
				else
					ret.second = true;
				return ret.first;
			}
	};
	/**
	 *
	 * @param beg
	 * @param end
	 * @param count
	 * @param value
	 * @return
	 */
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
		std::vector < std::future<ForwardIt> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, bool>> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		ForwardIt block_end2 = beg;
		ForwardIt last = end;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, static_cast<long>(count));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				threads[i] = std::thread(search_n_block<ForwardIt, Size, T>(), block_start,
										 block_end2, count, value, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			search_n_block<ForwardIt, Size, T>()(block_start, last, count, value,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, static_cast<long>(count));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				futures[i] = std::async(std::launch::async, search_n_block<ForwardIt, Size, T>(),
										block_start, block_end2, count, value, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			search_n_block<ForwardIt, Size, T>()(block_start, last, count, value,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future < ForwardIt > ::wait));
		}

		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}
	/**
	 *
	 * @param beg
	 * @param end
	 * @param count
	 * @param value
	 * @param p
	 * @return
	 */
	template<typename ForwardIt, typename Size, typename T, typename BinaryPredicate,
	typename Tpolicy = LaunchPolicies<ForwardIt> >
	ForwardIt search_n(ForwardIt beg, ForwardIt end, Size count, T& value, BinaryPredicate p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return end;
		if(!count)
			return beg;

		if(Tp.num_threads < 2)
			return std::search_n(beg, end, count, value, p);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<ForwardIt> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, bool>> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		ForwardIt block_end2 = beg;
		ForwardIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, static_cast<long>(count));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				threads[i] = std::thread(search_n_block2<ForwardIt, Size, T, BinaryPredicate>(),
										 block_start, block_end2, count, value, p, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			search_n_block2<ForwardIt, Size, T, BinaryPredicate>()(block_start, last, count, value,
					p, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, static_cast<long>(count));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				futures[i] = std::async(std::launch::async,
										search_n_block2<ForwardIt, Size, T, BinaryPredicate>(), block_start,
										block_end2, count, value, p, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			search_n_block2<ForwardIt, Size, T, BinaryPredicate>()(block_start, last, count, value,
					p, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future < ForwardIt > ::wait));
		}

		auto ans = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt, bool> v)->bool {return v.second;});
		if(ans == output.end())
			return end;
		else
			return ans->first;
	}

	template<typename ForwardIt1, typename ForwardIt2>
	struct find_end_block {
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param end2
			 * @param ret
			 * @param
			 * @param
			 * @return
			 */
			ForwardIt1 operator ()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
								   ForwardIt2 end2, std::pair<ForwardIt1, bool> &ret,
								   std::forward_iterator_tag, std::forward_iterator_tag) {

				ret.first = std::find_end(beg1, end1, beg2, end2);
				if(ret.first == end1)
					ret.second = false;
				else
					ret.second = true;
				return ret.first;
			}
	};

	template<typename ForwardIt1, typename ForwardIt2, typename BinaryOperator>
	struct find_end_block2 {
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param end2
			 * @param op
			 * @param ret
			 * @param
			 * @param
			 * @return
			 */
			ForwardIt1 operator ()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2,
								   ForwardIt2 end2, BinaryOperator op,
								   std::pair<ForwardIt1, bool> &ret, std::forward_iterator_tag,
								   std::forward_iterator_tag) {

				ret.first = std::find_end(beg1, end1, beg2, end2, op);
				if(ret.first == end1)
					ret.second = false;
				else
					ret.second = true;
				return ret.first;
			}
	};

	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param end2
	 * @return
	 */
	template<typename ForwardIt1, typename ForwardIt2, typename Tpolicy = LaunchPolicies<ForwardIt1> >
	ForwardIt1 find_end(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2, ForwardIt2 end2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return end1;
		if(beg2 == end2)
			return end1;
		if(Tp.num_threads < 2)
			return std::search(beg1, end1, beg2, end2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<ForwardIt1> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 block_end2 = beg1;
		ForwardIt1 last = end1;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, std::distance(beg2, end2));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				threads[i] = std::thread(find_end_block<ForwardIt1, ForwardIt2>(), block_start,
										 block_end2, beg2, end2, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt1>::iterator_category(),
										 typename std::iterator_traits<ForwardIt2>::iterator_category());
				block_start = block_end;
			}
			find_end_block<ForwardIt1, ForwardIt2>()(block_start, last, beg2, end2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt2>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, std::distance(beg2, end2));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				futures[i] = std::async(std::launch::async,
										find_end_block<ForwardIt1, ForwardIt2>(), block_start, block_end2, beg2,
										end2, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt1>::iterator_category(),
										typename std::iterator_traits<ForwardIt2>::iterator_category());
				block_start = block_end;
			}
			find_end_block<ForwardIt1, ForwardIt2>()(block_start, last, beg2, end2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt2>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future < ForwardIt1 > ::wait));
		}

		auto ans = std::find_if(output.rbegin(), output.rend(),
								[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.rend())
			return end1;
		else
			return ans->first;
	}
	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param end2
	 * @param op
	 * @return
	 */
	template<typename ForwardIt1, typename ForwardIt2, typename BinaryOperator,
	typename Tpolicy = LaunchPolicies<ForwardIt1> >
	ForwardIt1 find_end(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2, ForwardIt2 end2,
						BinaryOperator op) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return end1;
		if(beg2 == end2)
			return end1;
		if(Tp.num_threads < 2)
			return std::search(beg1, end1, beg2, end2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<ForwardIt1> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt1, bool>> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt1 block_end2 = beg1;
		ForwardIt1 last = end1;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, std::distance(beg2, end2));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				threads[i] = std::thread(find_end_block2<ForwardIt1, ForwardIt2, BinaryOperator>(),
										 block_start, block_end2, beg2, end2, op, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt1>::iterator_category(),
										 typename std::iterator_traits<ForwardIt1>::iterator_category());
				block_start = block_end;
			}
			find_end_block2<ForwardIt1, ForwardIt2, BinaryOperator>()(block_start, last, beg2, end2,
					op, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt2>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				block_end2 = block_end;
				std::advance(block_end2, std::distance(beg2, end2));
				if(std::distance(block_start, block_end2) >= std::distance(block_start, last))
					block_end2 = last;
				futures[i] = std::async(std::launch::async,
										find_end_block2<ForwardIt1, ForwardIt2, BinaryOperator>(), block_start,
										block_end2, beg2, end2, op, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt1>::iterator_category(),
										typename std::iterator_traits<ForwardIt1>::iterator_category());
				block_start = block_end;
			}
			find_end_block2<ForwardIt1, ForwardIt2, BinaryOperator>()(block_start, last, beg2, end2,
					op, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt1>::iterator_category(),
					typename std::iterator_traits<ForwardIt2>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future < ForwardIt1 > ::wait));
		}

		auto ans = std::find_if(output.rbegin(), output.rend(),
								[](std::pair<ForwardIt1, bool> v)->bool {return v.second;});
		if(ans == output.rend())
			return end1;
		else
			return ans->first;
	}

	template<typename InputIt1, typename InputIt2>
	struct mismatch_block {
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param ret
			 * @param
			 * @param
			 * @return
			 */
			std::pair<InputIt1, InputIt2> operator ()(
					InputIt1 beg1, InputIt1 end1, InputIt2 beg2,
					std::pair<std::pair<InputIt1, InputIt2>, bool> &ret, std::input_iterator_tag,
					std::input_iterator_tag) {

				ret.first = std::mismatch(beg1, end1, beg2);
				if(ret.first.first == end1)
					ret.second = false;
				else
					ret.second = true;
				return ret.first;
			}
	};
	/**
	 *
	 */
	template<typename InputIt1, typename InputIt2, typename BinaryOperator>
	struct mismatch_block2 {
			/**
			 *
			 * @param beg1
			 * @param end1
			 * @param beg2
			 * @param op
			 * @param ret
			 * @param
			 * @param
			 * @return
			 */
			std::pair<InputIt1, InputIt2> operator ()(
					InputIt1 beg1, InputIt1 end1, InputIt2 beg2, BinaryOperator op,
					std::pair<std::pair<InputIt1, InputIt2>, bool> &ret, std::input_iterator_tag,
					std::input_iterator_tag) {

				ret.first = std::mismatch(beg1, end1, beg2, op);
				if(ret.first.first == end1)
					ret.second = false;
				else
					ret.second = true;
				return ret.first;
			}
	};
	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @return
	 */
	template<typename InputIt1, typename InputIt2, typename Tpolicy = LaunchPolicies<InputIt1> >
	std::pair<InputIt1, InputIt2> mismatch(InputIt1 beg1, InputIt1 end1, InputIt2 beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return std::pair<InputIt1, InputIt2>(end1, beg1);

		if(Tp.num_threads < 2)
			return std::mismatch(beg1, end1, beg2);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<std::pair<InputIt1, InputIt2>> > futures(Tp.num_threads - 1);
		std::vector<std::pair<std::pair<InputIt1, InputIt2>, bool>> output(Tp.num_threads);
		InputIt1 block_start = beg1;
		InputIt1 block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt1 last = end1;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(mismatch_block<InputIt1, InputIt2>(), block_start,
										 block_end, block_start2, std::ref(output[i]),
										 typename std::iterator_traits<InputIt1>::iterator_category(),
										 typename std::iterator_traits<InputIt2>::iterator_category());
				std::advance(block_start2, Tp.block_size);
				block_start = block_end;
			}
			mismatch_block<InputIt1, InputIt2>()(block_start, last, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt1>::iterator_category(),
					typename std::iterator_traits<InputIt2>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, mismatch_block<InputIt1, InputIt2>(),
										block_start, block_end, block_start2, std::ref(output[i]),
										typename std::iterator_traits<InputIt1>::iterator_category(),
										typename std::iterator_traits<InputIt2>::iterator_category());
				std::advance(block_start2, Tp.block_size);
				block_start = block_end;
			}
			mismatch_block<InputIt1, InputIt2>()(block_start, last, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt1>::iterator_category(),
					typename std::iterator_traits<InputIt2>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future<std::pair<InputIt1, InputIt2>>::wait));
		}

		auto ans = std::find_if(output.rbegin(), output.rend(),
								[](std::pair<std::pair<InputIt1,InputIt2>, bool> v)->bool {return v.second;});
		if(ans == output.rend())
			return ans->first;
		else
			return ans->first;
	}
	/**
	 *
	 * @param beg1
	 * @param end1
	 * @param beg2
	 * @param op
	 * @return
	 */
	template<typename InputIt1, typename InputIt2, typename BinaryOperator,
	typename Tpolicy = LaunchPolicies<InputIt1> >
	std::pair<InputIt1, InputIt2> mismatch(InputIt1 beg1, InputIt1 end1, InputIt2 beg2,
			BinaryOperator op) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return std::pair<InputIt1, InputIt2>(end1, beg1);

		if(Tp.num_threads < 2)
			return std::mismatch(beg1, end1, beg2, op);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<std::pair<InputIt1, InputIt2>> > futures(Tp.num_threads - 1);
		std::vector<std::pair<std::pair<InputIt1, InputIt2>, bool>> output(Tp.num_threads);
		InputIt1 block_start = beg1;
		InputIt1 block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt1 last = end1;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(mismatch_block2<InputIt1, InputIt2, BinaryOperator>(),
										 block_start, block_end, block_start2, op, std::ref(output[i]),
										 typename std::iterator_traits<InputIt1>::iterator_category(),
										 typename std::iterator_traits<InputIt2>::iterator_category());
				std::advance(block_start2, Tp.block_size);
				block_start = block_end;
			}
			mismatch_block2<InputIt1, InputIt2, BinaryOperator>()(block_start, last, block_start2,
					op, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt1>::iterator_category(),
					typename std::iterator_traits<InputIt2>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										mismatch_block2<InputIt1, InputIt2, BinaryOperator>(), block_start,
										block_end, block_start2, op, std::ref(output[i]),
										typename std::iterator_traits<InputIt1>::iterator_category(),
										typename std::iterator_traits<InputIt2>::iterator_category());
				std::advance(block_start2, Tp.block_size);
				block_start = block_end;
			}
			mismatch_block2<InputIt1, InputIt2, BinaryOperator>()(block_start, last, block_start2,
					op, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt1>::iterator_category(),
					typename std::iterator_traits<InputIt2>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(&std::future<std::pair<InputIt1, InputIt2>>::wait));
		}

		auto ans = std::find_if(output.rbegin(), output.rend(),
								[](std::pair<std::pair<InputIt1,InputIt2>, bool> v)->bool {return v.second;});
		if(ans == output.rend())
			return ans->first;
		else
			return ans->first;
	}

	template<typename InputIt, typename T>
	struct count_block {
			/**
			 *
			 * @param beg
			 * @param end
			 * @param val
			 * @param ret
			 * @param
			 * @return
			 */
			typename std::iterator_traits<InputIt>::difference_type operator ()(
					InputIt beg, InputIt end, const T &val,
					typename std::iterator_traits<InputIt>::difference_type &ret,
					std::input_iterator_tag) {
				ret = std::count(beg, end, val);
				return ret;
			}
	};
	/**
	 * @summary counts all the items in the range with a certain value.
	 * @param beg Input iterator to the beginning of the input range
	 * @param end Input iterator to the end of the input range
	 * @param val value to be counted.
	 * @return the count of the item
	 */
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
		std::vector<std::future<typename std::iterator_traits<InputIt>::difference_type>> futures(
				Tp.num_threads - 1);
		std::vector<typename std::iterator_traits<InputIt>::difference_type> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(count_block<InputIt, T>(), block_start, block_end, val,
										 std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			count_block<InputIt, T>()(block_start, last, val, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, count_block<InputIt, T>(), block_start,
										block_end, val, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			count_block<InputIt, T>()(block_start, last, val, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(
								  &std::future<typename std::iterator_traits<InputIt>::difference_type>::wait));
		}

		auto ans = std::accumulate(output.begin(), output.end(), ret);
		return ans;
	}
	/**
	 * helper class for count_if
	 */
	template<typename InputIt, typename UnaryPredicate>
	struct count_if_block {
			/**
			 *
			 * @param beg Input iterator to the beginning of the input range
			 * @param end Input iterator to the end of the input range
			 * @param p unary predicate
			 * @param ret  used to hold the return values
			 * @param make sure we have Input iterators
			 * @return
			 */
			typename std::iterator_traits<InputIt>::difference_type operator ()(
					InputIt beg, InputIt end, UnaryPredicate p,
					typename std::iterator_traits<InputIt>::difference_type &ret,
					std::input_iterator_tag) {
				ret = std::count_if(beg, end, p);
				return ret;
			}
	};
	/**
	 * @summary counts all elements in the input range that satisfy the predicate
	 * @param beg Input iterator to the beginning of the input range
	 * @param end Input iterator to the end of the input range
	 * @param p unary predicate to be satisfied if we are to count the element
	 * @return the count of items that satisfy the unary predicate.
	 */
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
		std::vector<std::future<typename std::iterator_traits<InputIt>::difference_type>> futures(
				Tp.num_threads - 1);
		std::vector<typename std::iterator_traits<InputIt>::difference_type> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										count_if_block<InputIt, UnaryPredicate>(), block_start, block_end, p,
										std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			count_if_block<InputIt, UnaryPredicate>()(block_start, last, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(),
						  std::mem_fn(
								  &std::future<typename std::iterator_traits<InputIt>::difference_type>::wait));
		}

		auto ans = std::accumulate(output.begin(), output.end(), ret);
		return ans;
	}
	/**
	 * all of helper block.
	 */
	template<typename InputIt, typename UnaryPredicate>
	struct all_of_block {
			/**
			 *
			 * @param beg Input iterator to the beginning of the input block
			 * @param end Input iterator to the end of the input block
			 * @param p	unary predicate
			 * @param ret hold return value if all of the elements of the range satisfy the predicate.
			 * @param
			 */
			void operator ()(InputIt beg, InputIt end, UnaryPredicate p, int &ret,
							 std::input_iterator_tag) {
				auto val = std::all_of(beg, end, p);
				if(val)
					ret = 1;
				else
					ret = 0;

			}
	};
	/**
	 * @summary returs true if all the elements of the range satisfy the unary predicate.
	 * @param beg Input iterator to the beginning of the input range
	 * @param end Input iterator to the end of the input range
	 * @param p unary predicate
	 * @return
	 */
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
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, all_of_block<InputIt, UnaryPredicate>(),
										block_start, block_end, p, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			all_of_block<InputIt, UnaryPredicate>()(block_start, last, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());

			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		ret = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ret;
	}
	/***
	 * helper class for any_of
	 */
	template<typename InputIt, typename UnaryPredicate>
	struct any_of_block {
			/**
			 *
			 * @param beg Input iterator to the beginning of the input range
			 * @param end Input iterator to the end of the input range
			 * @param p Unary predicate
			 * @param ret The return value if any of the elements in the range are true
			 * @param
			 */
			void operator ()(InputIt beg, InputIt end, UnaryPredicate p, int &ret,
							 std::input_iterator_tag) {
				auto val = std::any_of(beg, end, p);
				if(val)
					ret = 1;
				else
					ret = 0;

			}
	};
	/**
	 * @summary returns true if any of the elements satisfy the predicate
	 *
	 * @param beg Input iterator to the begining of the input range
	 * @param end Input iterator to the end of the input range
	 * @param p unary predicate
	 * @return
	 */
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
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, any_of_block<InputIt, UnaryPredicate>(),
										block_start, block_end, p, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			any_of_block<InputIt, UnaryPredicate>()(block_start, last, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());

			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		ret = std::any_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ret;
	}
	template<typename InputIt, typename UnaryPredicate>
	struct none_of_block {
			/**
			 *
			 * @param beg Input iterator to the beginning of the input range
			 * @param end Input iterator to the end of the input range
			 * @param p unary predicate
			 * @param ret used for return value
			 * @param make sure we have input iterators
			 */
			void operator ()(InputIt beg, InputIt end, UnaryPredicate p, int &ret,
							 std::input_iterator_tag) {
				auto val = std::none_of(beg, end, p);
				if(val)
					ret = 1;
				else
					ret = 0;

			}
	};
	/**
	 *@summary function returns true if all values in the range do not satify the unary predicate.
	 *
	 * @param beg input iterator to the beginning of the input range
	 * @param end input iterator to the end of the input range.
	 * @param p Unary predicate used for criteria
	 * @return if this is true
	 */
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
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		InputIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										none_of_block<InputIt, UnaryPredicate>(), block_start, block_end, p,
										std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
			}
			none_of_block<InputIt, UnaryPredicate>()(block_start, last, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());

			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		ret = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ret;
	}
	/**
	 * equal helper class.
	 */
	template<typename InputIt1, typename InputIt2>
	struct equal_block {
			/**
			 * @summary helper for the equal function.
			 * @param beg1 input iterator to the beginning of the first input block
			 * @param end1 input iterator to the end of the first input block
			 * @param beg2 input iterator to the beginning of the second input block.
			 * @param retval
			 * @param make sure we have an input iterator
			 * @return if the two blocks are equal
			 */
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
	/**
	 * equal helper class.
	 */
	template<typename InputIt1, typename InputIt2>
	struct equal_block4 {
			/**
			 *
			 * @param beg1 input iterator to the beginning of the input block
			 * @param end1 input iterator to the end of the first input block
			 * @param beg2 input iterator to the beginning of the second input block
			 * @param end2 input iterator to the end of the second input block
			 * @param retval possible return value from the block.
			 * @param make sure we are using input iterators.
			 * @return
			 */
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
	/**
	 * @summary function to compare if two input ranges are equal.
	 * @param beg1 Input iterator to the beginning of the first input range
	 * @param end1 Input iterator to the end of the first input range
	 * @param beg2 Input iterator to the beginning of the second input range.
	 * @return true if the two input ranges are equal
	 */
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
		std::vector < std::future<bool> > futures(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt last = end1;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async, equal_block<InputIt, InputIt2>(),
										block_start, block_end, block_start2, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}
			equal_block<InputIt, InputIt2>()(block_start, last, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<bool>::wait));
		}

		auto ans = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ans;
	}

	template<typename InputIt1, typename InputIt2, typename BinaryPredicate>
	struct equal_block2 {
			/**
			 * @summary used to perform comparisons in each block.
			 * @param beg1 Input iterator to the beginning of the first input range
			 * @param end1 Input iterator to the end of the first input range
			 * @param beg2 Input iterator to the beginning of the second input range
			 * @param p binary predicate used for comparison
			 * @param retval return value from the block.
			 * @param make sure we are using input iterators.
			 * @return true if the two input ranges are equal.
			 */
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
	/**
	 * @summary if two iterator ranges are equal
	 * @param beg1 Input iterator to the beginning of the first range
	 * @param end1 Input iterator to the end of the first range
	 * @param beg2 Input iterator to the beginning of the second range
	 * @param p Binary predicate used for comparison.
	 * @return true if the ranges are equal.
	 */
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
		std::vector < std::future<bool> > futures(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt last = end1;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async,
										equal_block2<InputIt, InputIt2, BinaryPredicate>(), block_start, block_end,
										block_start2, p, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				std::advance(block_start2, Tp.block_size);
			}
			equal_block2<InputIt, InputIt2, BinaryPredicate>()(block_start, last, block_start2, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<bool>::wait));
		}

		auto ans = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ans;
	}
	/**
	 * helper class for block
	 */
	template<typename InputIt1, typename InputIt2, typename BinaryPredicate>
	struct equal_block3 {
			/**
			 *
			 * @param beg1	Input iterator to the beginning of the first input range
			 * @param end1 Input iterator to the end of the first input range
			 * @param beg2 Input iterator to the beginning of the second input range
			 * @param end2 Input iterator to the end of the second input range
			 * @param p Binary predicate used for comparison
			 * @param retval possible return value from the block
			 * @param make sure we have input iterators.
			 * @return boolean value if the blocks are equal.
			 */
			bool operator()(InputIt1 beg1, InputIt1 end1, InputIt2 beg2, InputIt2 end2,
							BinaryPredicate p, int &retval, std::input_iterator_tag) {
				auto ans = std::equal(beg1, end1, beg2, end2, p);
				if(ans)
					retval = 1;
				else
					retval = 0;
				return ans;
			}
	};
	/**
	 *
	 * @param beg1 Input iterator to the beginning of the first input range
	 * @param end1 Input iterator to the end of the first input range
	 * @param beg2 Input iterator to the beginning of the second input range
	 * @param end2 Input iterator to the end of the second input rage
	 * @param p Binary predicate used for comparison.
	 * @return
	 */
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
		std::vector < std::future<bool> > futures(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt2 block_end2 = beg2;
		InputIt last = end1;
		InputIt2 last2 = end2;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				std::advance(block_end2, Tp.block_size);
				threads[i] = std::thread(equal_block3<InputIt, InputIt2, BinaryPredicate>(),
										 block_start, block_end, block_start2, block_end2, p, std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				block_start2 = block_end2;
			}
			equal_block3<InputIt, InputIt2, BinaryPredicate>()(block_start, last, block_start2,
					last2, p, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				std::advance(block_end2, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										equal_block3<InputIt, InputIt2, BinaryPredicate>(), block_start, block_end,
										block_start2, block_end2, p, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				block_start2 = block_end2;
			}
			equal_block3<InputIt, InputIt2, BinaryPredicate>()(block_start, last, block_start2,
					last2, p, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<bool>::wait));
		}

		auto ans = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ans;
	}

	//equal version 4
	/**
	 *
	 * @param beg1 Input iterator to the beginning of the first input range
	 * @param end1 Input iterator to the end of the first input range
	 * @param beg2 Input iterator to the beginning of the second input range
	 * @param end2 Input iterator to the end of the second input range
	 * @return
	 */
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
		std::vector<std::future<bool>> futures(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		InputIt block_start = beg1;
		InputIt block_end = beg1;
		InputIt2 block_start2 = beg2;
		InputIt2 block_end2 = beg2;
		InputIt last = end1;
		InputIt2 last2 = end2;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				std::advance(block_end2, Tp.block_size);
				futures[i] = std::async(std::launch::async, equal_block4<InputIt, InputIt2>(),
										block_start, block_end, block_start2, block_end2, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				block_start = block_end;
				block_start2 = block_end2;
			}
			equal_block4<InputIt, InputIt2>()(block_start, last, block_start2, last2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<bool>::wait));
		}

		auto ans = std::all_of(output.begin(), output.end(), [](int k)->bool {return k==1;});
		return ans;
	}

	template<typename ForwardIt>
	struct max_element_block {
			/**
			 *
			 * @param beg iterator to the beginning of the block
			 * @param end iterator to the end of the block
			 * @param retval possible return value from the block.
			 * @param
			 */
			void operator()(
					ForwardIt beg,
					ForwardIt end,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retval,
					std::forward_iterator_tag) {
				retval.first = (std::max_element(beg, end));
				retval.second = *(retval.first);
			}
	};
	/**
	 *
	 * @param beg Forward iterator to the beginning of the input container range
	 * @param end Forward iterator to the end of the input container range
	 * @return
	 */
	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >

	ForwardIt max_element(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2)
			return std::max_element(beg, end);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(max_element_block<ForwardIt>(), block_start, block_end,
										 std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			max_element_block<ForwardIt>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async, max_element_block<ForwardIt>(),
										block_start, block_end, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			max_element_block<ForwardIt>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		auto ans =
				std::max_element(output.begin(), output.end(),
								 [](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
										 std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return a.second<b.second;});
		return (*ans).first;
	}
	/**
	 * max_element block helper class.
	 */
	template<typename ForwardIt, typename Comp>
	struct max_element_block2 {
			/**
			 *
			 * @param beg iterator to the beginning of the input block
			 * @param end iterator to the end of the input block
			 * @param cmp binary comparison predicate
			 * @param retval return value for this block.
			 * @param
			 */
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
	/**
	 *
	 * @param beg Forward iterator to the beginning of the input container
	 * @param end Forward iterator to the end of the input container
	 * @param cmp binary comparison operator.
	 * @return Forward iterator to the max element.
	 */
	template<typename ForwardIt, typename Comp, typename Tpolicy = LaunchPolicies<ForwardIt> >

	ForwardIt max_element(ForwardIt beg, ForwardIt end, Comp cmp) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2)
			return std::max_element(beg, end, cmp);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(max_element_block2<ForwardIt, Comp>(), block_start,
										 block_end, cmp, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			max_element_block2<ForwardIt, Comp>()(block_start, last, cmp,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async, max_element_block2<ForwardIt, Comp>(),
										block_start, block_end, cmp, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			max_element_block2<ForwardIt, Comp>()(block_start, last, cmp,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		auto ans =
				std::max_element(output.begin(), output.end(),
								 [&](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
										 std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return cmp(a.second,b.second);});
		return (*ans).first;
	}
	/**
	 * min_element helper class.
	 */
	template<typename ForwardIt>
	struct min_element_block {
			/**
			 *
			 * @param beg Forward iterator to the beginning of input block
			 * @param end Forward iterator to the end of the input block
			 * @param retval possible return value from this block
			 * @param to make sure we have forward iterators.
			 */
			void operator()(
					ForwardIt beg,
					ForwardIt end,
					std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> &retval,
					std::forward_iterator_tag) {
				retval.first = (std::min_element(beg, end));
				retval.second = *(retval.first);
			}
	};
	/**
	 *
	 * @param beg Forward iterator to the beginning of the input container
	 * @param end Forward iterator to the end of the input container
	 * @return Forward iterator to the minimum element
	 */
	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >

	ForwardIt min_element(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2)
			return std::min_element(beg, end);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(min_element_block<ForwardIt>(), block_start, block_end,
										 std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			min_element_block<ForwardIt>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async, min_element_block<ForwardIt>(),
										block_start, block_end, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			min_element_block<ForwardIt>()(block_start, last, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		auto ans =
				std::min_element(output.begin(), output.end(),
								 [](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
										 std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return a.second<b.second;});
		return (*ans).first;
	}
	/**
	 * min element helper class.
	 */
	template<typename ForwardIt, typename Comp>
	struct min_element_block2 {
			/**
			 *
			 * @param beg iterator to the beginning of the block
			 * @param end iterator to the end of the block
			 * @param cmp binary comparison operator
			 * @param retval value returned from each block
			 * @param make sure that we have forward iterators.
			 */
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
	/**
	 *
	 * @param beg Forward iterator to the beginning of the input container
	 * @param end Forward iterator to the end of the input container
	 * @param cmp binary comparison operator for container elements
	 * @return Iterator to the minimum element.
	 */
	template<typename ForwardIt, typename Comp, typename Tpolicy = LaunchPolicies<ForwardIt> >

	ForwardIt min_element(ForwardIt beg, ForwardIt end, Comp cmp) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2)
			return std::min_element(beg, end, cmp);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(min_element_block2<ForwardIt, Comp>(), block_start,
										 block_end, cmp, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			min_element_block2<ForwardIt, Comp>()(block_start, last, cmp,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async, min_element_block2<ForwardIt, Comp>(),
										block_start, block_end, cmp, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			min_element_block2<ForwardIt, Comp>()(block_start, last, cmp,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		auto ans =
				std::min_element(output.begin(), output.end(),
								 [&](std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> a,
										 std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> b)->bool {return cmp(a.second,b.second);});
		return (*ans).first;
	}

	/**
	 * Helper function for minmax_element.
	 */
	template<typename ForwardIt>
	struct minmax_element_block {
			/**
			 *
			 * @param beg the beginning of the block.
			 * @param end the end of the block.
			 * @param retmin the minimum in this block.
			 * @param retmax the maximum in the block.
			 * @param make sure we have a forward iterator.
			 */
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
	/**
	 *
	 * @param beg Forward iterator to the beginning of the input container.
	 * @param end Forward iterator to the end of the input container
	 * @return std::pair containing the minimum and maximum element
	 */
	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >

	std::pair<ForwardIt, ForwardIt> minmax_element(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return std::pair<ForwardIt, ForwardIt>(end, end);
		if(Tp.num_threads < 2)
			return std::minmax_element(beg, end);
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output1(
				Tp.num_threads);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type> > output2(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(minmax_element_block<ForwardIt>(), block_start, block_end,
										 std::ref(output1[i]), std::ref(output2[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			minmax_element_block<ForwardIt>()(block_start, last,
					std::ref(output1[Tp.num_threads - 1]), std::ref(output2[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async, minmax_element_block<ForwardIt>(),
										block_start, block_end, std::ref(output1[i]), std::ref(output2[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			minmax_element_block<ForwardIt>()(block_start, last,
					std::ref(output1[Tp.num_threads - 1]), std::ref(output2[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

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

	/**
	 * Helper methods for computing the minmax_element block by block
	 */
	template<typename ForwardIt, typename Comp>
	struct minmax_element_block2 {
			/**
			 *
			 * @param beg input iterator to the start of the range to consider in the input container.
			 * @param end input iterator to the end of the range to consider in the input container
			 * @param cmp the comparison function
			 * @param retmin iterator for the min in this block
			 * @param retmax iterator for the max in this block
			 * @param
			 */
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
	/**
	 *
	 * @param beg start of the range to consider for the input container.
	 * @param end end of the range to consider in the input container
	 * @param cmp comparison function for two elements
	 * @return std::pair<ForwardIt,ForwardIt> a pair of iterators to the min and max element in the container
	 */
	template<typename ForwardIt, typename Comp, typename Tpolicy = LaunchPolicies<ForwardIt> >

	std::pair<ForwardIt, ForwardIt> minmax_element(ForwardIt beg, ForwardIt end, Comp cmp) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return std::pair<ForwardIt, ForwardIt>(end, end);

		if(Tp.num_threads < 2)
			return std::minmax_element(beg, end);

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type>> output1(
				Tp.num_threads);
		std::vector<std::pair<ForwardIt, typename std::iterator_traits<ForwardIt>::value_type>> output2(
				Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		ForwardIt last = end;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

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
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async,
										minmax_element_block2<ForwardIt, Comp>(), block_start, block_end, cmp,
										std::ref(output1[i]), std::ref(output2[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;

			}
			minmax_element_block2<ForwardIt, Comp>()(block_start, last, cmp,
					std::ref(output1[Tp.num_threads - 1]), std::ref(output2[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

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

	/**
	 * Helper functions for doing the copy_if block by block.
	 */
	template<typename InputIt, typename OutputIt, typename UnaryPred>
	struct copy_if_block {
			/**
			 *
			 * @param beg1	input iterator to the beginning of the range of the input container
			 * @param end1  input iterator to the end of the range of the input container
			 * @param p		unary predicate that determines if the element should be copied
			 * @param ret   std::vector<InputIt> holds all the iterators to the elements to be copied
			 * @param
			 */
			void operator()(InputIt beg1, InputIt end1, UnaryPred p, std::vector<InputIt> &ret,
							std::input_iterator_tag) {
				InputIt first = beg1;
				ret = std::vector<InputIt>();

				while(first != end1) {
					if(p(*first))
						ret.push_back(first);
					first++;
				}
			}

			/**
			 * @summary This version copies in all the values of the iterators that statisfy the required predicate into the Output Iterator.
			 *
			 * @param beg2 beginning of the Output container
			 * @param ret vector containing Iterators to elements that satisfy the predicate.
			 * @param make sure we have at leas an input iterator.
			 * @return
			 */
			void operator()(OutputIt beg2, std::vector<InputIt> &ret, std::input_iterator_tag) {
				typename std::vector<InputIt>::iterator first = ret.begin();
				typename std::vector<InputIt>::iterator end = ret.end();

				while(first != end) {
					*beg2++ = *(*first);
					first++;
				}
			}
	};

	/**
	 * Helper functions for doing the copy block .
	 */
	template<typename InputIt, typename OutputIt>
	struct copy_block {
			/**
			 *
			 * @param beg1	input iterator to the beginning of the range of the input container
			 * @param end1  input iterator to the end of the range of the input container
			 * @param beg2  the beginning of the output iterator block
			 * @param ret reference to a pair used as return values
			 * @param
			 */
			void operator()(InputIt beg1, InputIt end1, OutputIt beg2,
							std::pair<OutputIt, bool>&ret, std::input_iterator_tag) {
				ret.first = std::copy(beg1, end1, beg2);
				ret.second = true;
			}

	};

	/**
	 *
	 * @param beg input iterator to the start of the range to be copied.
	 * @param end input iterator to the end of the range to be copied.
	 * @param beg2 output iterator to the beginning of the output container which must at least be
	 * 					of length end -beg
	 * @return
	 */
	template<typename InputIt, typename OutputIt, typename Tpolicy = LaunchPolicies<InputIt> >
	OutputIt copy(InputIt beg, InputIt end, OutputIt beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2) {
			return std::copy(beg, end, beg2);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<OutputIt, bool>> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		OutputIt block_start2 = beg2;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(copy_block<InputIt, OutputIt>(), block_start, block_end,
										 block_start2, std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;
				std::advance(block_start2, Tp.block_size);

			}
			copy_block<InputIt, OutputIt>()(block_start, end, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async, copy_block<InputIt, OutputIt>(),
										block_start, block_end, block_start2, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;
				std::advance(block_start2, Tp.block_size);

			}
			copy_block<InputIt, OutputIt>()(block_start, end, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		OutputIt last = (output[Tp.num_threads - 1]).first;

		return last;
	}

	/**
	 * Helper functions for doing the reverse copy block .
	 */
	template<typename BidirIt, typename OutputIt>
	struct reverse_copy_block {
			/**
			 *
			 * @param beg1	input iterator to the beginning of the range of the input container
			 * @param end1  input iterator to the end of the range of the input container
			 * @param beg2  the beginning of the output iterator block
			 * @param ret reference to a pair used as return values
			 * @param
			 */
			void operator()(BidirIt beg1, BidirIt end1, OutputIt beg2,
							std::pair<OutputIt, bool>&ret, std::bidirectional_iterator_tag) {
				ret.first = std::reverse_copy(beg1, end1, beg2);
				ret.second = true;
			}

	};

	/**
	 *
	 * @param beg input iterator to the start of the range to be copied.
	 * @param end input iterator to the end of the range to be copied.
	 * @param beg2 output iterator to the beginning of the output container which must at least be
	 * 					of length end -beg
	 * @return
	 */
	template<typename BidirIt, typename OutputIt, typename Tpolicy = LaunchPolicies<BidirIt> >
	OutputIt reverse_copy(BidirIt beg, BidirIt end, OutputIt beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2) {
			return std::reverse_copy(beg, end, beg2);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<OutputIt, bool>> output(Tp.num_threads);
		BidirIt block_start = end;
		BidirIt block_end = end;
		OutputIt block_start2 = beg2;
		block_end = end;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_start, -Tp.block_size);

				threads[i] = std::thread(reverse_copy_block<BidirIt, OutputIt>(), block_start,
										 block_end, block_start2, std::ref(output[i]),
										 typename std::iterator_traits<BidirIt>::iterator_category());

				block_end = block_start;
				std::advance(block_start2, Tp.block_size);

			}
			reverse_copy_block<BidirIt, OutputIt>()(beg, block_end, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<BidirIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_start, -Tp.block_size);

				futures[i] = std::async(std::launch::async, reverse_copy_block<BidirIt, OutputIt>(),
										block_start, block_end, block_start2, std::ref(output[i]),
										typename std::iterator_traits<BidirIt>::iterator_category());

				block_end = block_start;
				std::advance(block_start2, Tp.block_size);

			}
			copy_block<BidirIt, OutputIt>()(beg, block_end, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<BidirIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		OutputIt last = (output[Tp.num_threads - 1]).first;

		return last;
	}

	/**
	 * Helper functions for doing the copy block .
	 */
	template<typename InputIt, typename Size, typename OutputIt>
	struct copy_n_block {
			/**
			 *
			 * @param beg1	input iterator to the beginning of the range of the input container
			 * @param count	the number of consective copies to be made for this block.
			 * @param beg2  the beginning of the output iterator block
			 * @param ret reference to a pair used as return values
			 * @param
			 */
			void operator()(InputIt beg1, Size count, OutputIt beg2, std::pair<OutputIt, bool>&ret,
							std::input_iterator_tag) {
				ret.first = std::copy_n(beg1, count, beg2);
				ret.second = true;
			}

	};

	/**
	 *
	 * @param beg input iterator to the start of the range to be copied.
	 * @param end input iterator to the end of the range to be copied.
	 * @param beg2 output iterator to the beginning of the output container which must at least be
	 * 					of length end -beg
	 * @return
	 */
	template<typename InputIt, typename Size, typename OutputIt, typename Tpolicy = LaunchPolicies<
			InputIt> >
	OutputIt copy_n(InputIt beg, Size count, OutputIt beg2) {
		Tpolicy Tp;
		InputIt end = beg;
		std::advance(end, count);
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg2;
		if(!count)
			return beg2;
		if(Tp.num_threads < 2) {
			return std::copy_n(beg, count, beg2);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::future<void>> futures(Tp.num_threads - 1);
		std::vector<std::pair<OutputIt, bool>> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		OutputIt block_start2 = beg2;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				Size cnt = static_cast<Size>((block_end - block_start));
				threads[i] = std::thread(copy_n_block<InputIt, Size, OutputIt>(), block_start, cnt,
										 block_start2, std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;
				std::advance(block_start2, Tp.block_size);

			}
			Size cnt2 = static_cast<Size>(end - block_start);
			copy_n_block<InputIt, Size, OutputIt>()(block_start, cnt2, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				Size cnt = static_cast<Size>((block_end - block_start));
				futures[i] = std::async(std::launch::async, copy_n_block<InputIt, Size, OutputIt>(),
										block_start, cnt, block_start2, std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;
				std::advance(block_start2, Tp.block_size);

			}
			Size cnt2 = static_cast<Size>(end - block_start);
			copy_n_block<InputIt, Size, OutputIt>()(block_start, cnt2, block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		OutputIt last = (output[Tp.num_threads - 1]).first;

		return last;
	}

	/**
	 *
	 * @param beg input iterator to the start of the range to be copied.
	 * @param end input iterator to the end of the range to be copied.
	 * @param beg2 output iterator to the beginning of the output container which must at least be
	 * 					of length end -beg
	 * @param p			Unary predicate that acts on each element of the input container if it should be copied.
	 * @return
	 */
	template<typename InputIt, typename OutputIt, typename UnaryPred,
	typename Tpolicy = LaunchPolicies<InputIt> >
	OutputIt copy_if(InputIt beg, InputIt end, OutputIt beg2, UnaryPred p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2) {
			return std::copy_if(beg, end, beg2, p);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::thread > threads2(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector < std::future<void> > futures2(Tp.num_threads - 1);
		std::vector<std::vector<InputIt>> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				threads[i] = std::thread(copy_if_block<InputIt, OutputIt, UnaryPred>(), block_start,
										 block_end, p, std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;

			}
			copy_if_block<InputIt, OutputIt, UnaryPred>()(block_start, end, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

			//redo for copy. not exactly load balanced but works :)
			OutputIt block_start2 = beg2;
			OutputIt block_end2 = beg2;
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end2, output[i].size());
				threads2[i] = std::thread(copy_if_block<InputIt, OutputIt, UnaryPred>(),
										  block_start2, std::ref(output[i]),
										  typename std::iterator_traits<InputIt>::iterator_category());

				block_start2 = block_end2;
			}
			copy_if_block<InputIt, OutputIt, UnaryPred>()(block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads2.begin(), threads2.end(), std::mem_fn(&std::thread::join));
		}

		//same for async
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);

				futures[i] = std::async(std::launch::async,
										copy_if_block<InputIt, OutputIt, UnaryPred>(), block_start, block_end, p,
										std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());

				block_start = block_end;

			}
			copy_if_block<InputIt, OutputIt, UnaryPred>()(block_start, end, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));

			//redo for copy. not exactly load balanced but works :)
			OutputIt block_start2 = beg2;
			OutputIt block_end2 = beg2;
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end2, output[i].size());
				futures2[i] = std::async(std::launch::async,
										 copy_if_block<InputIt, OutputIt, UnaryPred>(), block_start2,
										 std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());

				block_start2 = block_end2;
			}
			copy_if_block<InputIt, OutputIt, UnaryPred>()(block_start2,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures2.begin(), futures2.end(), std::mem_fn(&std::future<void>::wait));
		}

		auto ans = std::accumulate(output.begin(), output.end(), 0,
								   [&](int & val, std::vector<InputIt> & vec)->int {return val+ vec.size();});

		OutputIt last = beg2;
		std::advance(last, ans);
		return last;
	}

	/**
	 * handles each block of the replace function.
	 */
	template<typename ForwardIt, typename T>
	struct replace_block {
			/**
			 *
			 * @param beg ForwardIterator to the beginning of the input container
			 * @param end ForwardIterator to the end of the input container
			 * @param old_value the old value to be replaced
			 * @param new_value the new value with which we replace the old value
			 * @param to make sure we have
			 */
			void operator ()(ForwardIt beg, ForwardIt end, const T& old_value, const T& new_value,
							 std::forward_iterator_tag) {
				std::replace(beg, end, old_value, new_value);
			}
	};
	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @param old_value the old value to be replaced
	 * @param new_value the new value with which we replace the old value
	 *
	 */
	template<typename ForwardIt, typename T, typename Tpolicy = LaunchPolicies<ForwardIt> >
	void replace(ForwardIt beg, ForwardIt end, const T& old_value, const T& new_value) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return;
		if(Tp.num_threads < 2) {
			std::replace(beg, end, old_value, new_value);
			return;
		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(replace_block<ForwardIt, T>(), block_start, block_end,
										 std::ref(old_value), std::ref(new_value),
										 typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			replace_block<ForwardIt, T>()(block_start, end, std::ref(old_value),
					std::ref(new_value),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, replace_block<ForwardIt, T>(),
										block_start, block_end, std::ref(old_value), std::ref(new_value),
										typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			replace_block<ForwardIt, T>()(block_start, end, std::ref(old_value),
					std::ref(new_value),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

	}
	/**
	 * handles each block of the replace_if function.
	 */
	template<typename ForwardIt, typename UnaryPred, typename T>
	struct replace_if_block {
			/**
			 *
			 * @param beg ForwardIterator to the beginning of the input container
			 * @param end ForwardIterator to the end of the input container
			 * @param UnaryPred unary predicate that determines which element to replace
			 * @param new_value the new value with which we replace the old value
			 * @param to make sure we have a forward iterator
			 */
			void operator ()(ForwardIt beg, ForwardIt end, UnaryPred p, const T& new_value,
							 std::forward_iterator_tag) {
				std::replace_if(beg, end, p, new_value);
			}
	};

	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @param p unary predicate which determines what to replace if pred(val)==true.
	 * @param new_value the new value with which we replace the old value
	 *
	 */
	template<typename ForwardIt, typename UnaryPred, typename T, typename Tpolicy = LaunchPolicies<
			ForwardIt> >
	void replace_if(ForwardIt beg, ForwardIt end, UnaryPred p, const T& new_value) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return;
		if(Tp.num_threads < 2) {
			std::replace_if(beg, end, p, new_value);
			return;
		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(replace_if_block<ForwardIt, UnaryPred, T>(), block_start,
										 block_end, p, std::ref(new_value),
										 typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			replace_if_block<ForwardIt, UnaryPred, T>()(block_start, end, p, std::ref(new_value),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										replace_if_block<ForwardIt, UnaryPred, T>(), block_start, block_end, p,
										std::ref(new_value),
										typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			replace_if_block<ForwardIt, UnaryPred, T>()(block_start, end, p, std::ref(new_value),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

	}

	/**
	 * handles each block of the replace_copy function.
	 */
	template<typename InputIt, typename OutputIt, typename T>
	struct replace_copy_block {
			/**
			 *
			 * @param beg InputIterator to the beginning of the input container
			 * @param end InputIterator to the end of the input container
			 * @param beg2 OutputIterator to the beginning of the output container
			 * @param old_value the old value to be replaced
			 * @param new_value the new value with which we replace the old value
			 * @param to make sure we have an input iterator in the container.
			 */
			void operator ()(InputIt beg, InputIt end, OutputIt beg2, const T& old_value,
							 const T& new_value, std::pair<OutputIt, bool> &ret,
							 std::input_iterator_tag) {
				ret.first = std::replace_copy(beg, end, beg2, old_value, new_value);
				ret.second = true;
			}
	};

	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @param beg2 Iterator to the beginning of the output container
	 * @param old_value the old value to be replaced when copied
	 * @param new_value the new value with which we replace the old value
	 * @returns OutputIt
	 */
	template<typename InputIt, typename OutputIt, typename T, typename Tpolicy = LaunchPolicies<
			InputIt> >
	OutputIt replace_copy(InputIt beg, InputIt end, OutputIt beg2, const T& old_value,
						  const T& new_value) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2) {
			return std::replace_copy(beg, end, beg2, old_value, new_value);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::future<void>> futures(Tp.num_threads - 1);
		std::vector<std::pair<OutputIt, bool>> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		OutputIt block_start2 = beg2;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(replace_copy_block<InputIt, OutputIt, T>(), block_start,
										 block_end, block_start2, std::ref(old_value), std::ref(new_value),
										 std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				std::advance(block_start2, Tp.block_size);
				block_start = block_end;
			}
			replace_copy_block<InputIt, OutputIt, T>()(block_start, end, block_start2,
					std::ref(old_value), std::ref(new_value), std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										replace_copy_block<InputIt, OutputIt, T>(), block_start, block_end,
										block_start2, std::ref(old_value), std::ref(new_value), std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				std::advance(block_start2, Tp.block_size);
				block_start = block_end;
			}
			replace_copy_block<InputIt, OutputIt, T>()(block_start, end, block_start2,
					std::ref(old_value), std::ref(new_value), std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		return (output[Tp.num_threads - 1]).first;

	}

	/**
	 * handles each block of the replace_copy function.
	 */
	template<typename InputIt, typename OutputIt, typename UnaryPred, typename T>
	struct replace_copy_if_block {
			/**
			 *
			 * @param beg InputIterator to the beginning of the input container
			 * @param end InputIterator to the end of the input container
			 * @param beg2 OutputIterator to the beginning of the output container
			 * @param p	unary predicate determining if the value being copied should be replaced
			 * @param new_value the new value with which we replace the old value when copied.
			 * @param to make sure we have an input iterator in the container.
			 */
			void operator ()(InputIt beg, InputIt end, OutputIt beg2, UnaryPred p,
							 const T& new_value, std::pair<OutputIt, bool> &ret,
							 std::input_iterator_tag) {
				ret.first = std::replace_copy_if(beg, end, beg2, p, new_value);
				ret.second = true;
			}
	};

	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @param beg2 Iterator to the beginning of the output container
	 * @param p unary predicate that determines which element needs to be replaced when copied.
	 * @param new_value the new value with which we replace the old value
	 * @returns OutputIt
	 */
	template<typename InputIt, typename OutputIt, typename UnaryPred, typename T,
	typename Tpolicy = LaunchPolicies<InputIt> >
	OutputIt replace_copy_if(InputIt beg, InputIt end, OutputIt beg2, UnaryPred p,
							 const T& new_value) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2) {
			return std::replace_copy_if(beg, end, beg2, p, new_value);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<OutputIt, bool>> output(Tp.num_threads);
		InputIt block_start = beg;
		InputIt block_end = beg;
		OutputIt block_start2 = beg2;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(replace_copy_if_block<InputIt, OutputIt, UnaryPred, T>(),
										 block_start, block_end, block_start2, p, std::ref(new_value),
										 std::ref(output[i]),
										 typename std::iterator_traits<InputIt>::iterator_category());
				std::advance(block_start2, Tp.block_size);
				block_start = block_end;
			}
			replace_copy_if_block<InputIt, OutputIt, UnaryPred, T>()(block_start, end, block_start2,
					p, std::ref(new_value), std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										replace_copy_if_block<InputIt, OutputIt, UnaryPred, T>(), block_start,
										block_end, block_start2, p, std::ref(new_value), std::ref(output[i]),
										typename std::iterator_traits<InputIt>::iterator_category());
				std::advance(block_start2, Tp.block_size);
				block_start = block_end;
			}
			replace_copy_if_block<InputIt, OutputIt, UnaryPred, T>()(block_start, end, block_start2,
					p, std::ref(new_value), std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<InputIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		return (output[Tp.num_threads - 1]).first;

	}
	/**
	 * handles each block of the is_sorted function.
	 */
	template<typename ForwardIt>
	struct is_sorted_block {
			/**
			 *
			 * @param beg ForwardIterator to the beginning of the input container
			 * @param end ForwardIterator to the end of the input container
			 * @param beg2 OutputIterator to the beginning of the output container
			 * @param used to return the result of the block computation.
			 * @param to make sure we have an forward iterator to the container.
			 */
			void operator ()(ForwardIt beg, ForwardIt end, int &ret, std::forward_iterator_tag) {
				auto val = std::is_sorted(beg, end);
				if(val)
					ret = 1;
				else
					ret = 0;

			}
	};
	/**
	 *  Helper for the is_sorted function with a binary predicate.
	 */
	template<typename ForwardIt, typename BinaryPred>
	struct is_sorted_block2 {
			/**
			 *
			 * @param beg ForwardIterator to the beginning of the input container
			 * @param end ForwardIterator to the end of the input container
			 * @param beg2 OutputIterator to the beginning of the output container
			 * @param p predicate used for comparison.
			 * @param used to return the result of the block computation.
			 * @param to make sure we have an forward iterator to the container.
			 */
			void operator ()(ForwardIt beg, ForwardIt end, BinaryPred p, int &ret,
							 std::forward_iterator_tag) {
				auto val = std::is_sorted(beg, end, p);
				if(val)
					ret = 1;
				else
					ret = 0;
			}
	};
	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @returns true if the container is sorted false it it is not.
	 */
	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >
	bool is_sorted(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return true;
		if(Tp.num_threads < 2) {
			return std::is_sorted(beg, end);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(is_sorted_block<ForwardIt>(), block_start, (block_end + 1),
										 std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			is_sorted_block<ForwardIt>()(block_start, end, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, is_sorted_block<ForwardIt>(),
										block_start, (block_end + 1), std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			is_sorted_block<ForwardIt>()(block_start, end, std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		return std::all_of(output.begin(), output.end(), [](int v)->bool {return v==1;});

	}

	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @param p Binary predicate used to compare elements.
	 * @returns true if the container is sorted false it it is not.
	 */
	template<typename ForwardIt, typename BinaryPred, typename Tpolicy = LaunchPolicies<ForwardIt> >
	bool is_sorted(ForwardIt beg, ForwardIt end, BinaryPred p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return true;
		if(Tp.num_threads < 2) {
			return std::is_sorted(beg, end, p);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<int> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(is_sorted_block2<ForwardIt, BinaryPred>(), block_start,
										 (block_end + 1), p, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			is_sorted_block2<ForwardIt, BinaryPred>()(block_start, end, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async,
										is_sorted_block2<ForwardIt, BinaryPred>(), block_start, (block_end + 1), p,
										std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			is_sorted_block2<ForwardIt, BinaryPred>()(block_start, end, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		return std::all_of(output.begin(), output.end(), [](int v)->bool {return v==1;});

	}

	/**
	 * handles each block of the is_sorted function.
	 */
	template<typename ForwardIt>
	struct is_sorted_until_block {
			/**
			 *
			 * @param beg ForwardIterator to the beginning of the input container
			 * @param end ForwardIterator to the end of the input container
			 * @param beg2 OutputIterator to the beginning of the output container
			 * @param used to return the result of the block computation.
			 * @param to make sure we have an forward iterator to the container.
			 */
			void operator ()(ForwardIt beg, ForwardIt end, std::pair<ForwardIt, int> &ret,
							 std::forward_iterator_tag) {
				auto val = std::is_sorted_until(beg, end);
				if(val == end) {
					ret.second = 1;
					ret.first = val;
				}
				else {
					ret.second = 0;
					ret.first = val;
				}

			}
	};
	/**
	 *  Helper for the is_sorted function with a binary predicate.
	 */
	template<typename ForwardIt, typename BinaryPred>
	struct is_sorted_until_block2 {
			/**
			 *
			 * @param beg ForwardIterator to the beginning of the input container
			 * @param end ForwardIterator to the end of the input container
			 * @param beg2 OutputIterator to the beginning of the output container
			 * @param p predicate used for comparison.
			 * @param used to return the result of the block computation.
			 * @param to make sure we have an forward iterator to the container.
			 */
			void operator ()(ForwardIt beg, ForwardIt end, BinaryPred p,
							 std::pair<ForwardIt, int> &ret, std::forward_iterator_tag) {
				auto val = std::is_sorted_until(beg, end, p);
				if(val == end) {
					ret.second = 1;
					ret.first = val;
				}
				else {
					ret.second = 0;
					ret.first = val;
				}
			}
	};

	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @returns iterator past the end of last sorted element.
	 */
	template<typename ForwardIt, typename Tpolicy = LaunchPolicies<ForwardIt> >
	ForwardIt is_sorted_until(ForwardIt beg, ForwardIt end) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2) {
			return std::is_sorted_until(beg, end);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, int>> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(is_sorted_until_block<ForwardIt>(), block_start,
										 (block_end + 1), std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			is_sorted_until_block<ForwardIt>()(block_start, end,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, is_sorted_until_block<ForwardIt>(),
										block_start, (block_end + 1), std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());
				block_start = block_end;
			}
			is_sorted_until_block<ForwardIt>()(block_start, end,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		auto val = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt,int> &v)->bool {return v.second==0;});
		return val->first;

	}

	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @param p Binary predicate used to compare elements.
	 * @returns iterator past the end of the last sorted element.
	 */
	template<typename ForwardIt, typename BinaryPred, typename Tpolicy = LaunchPolicies<ForwardIt> >
	ForwardIt is_sorted_until(ForwardIt beg, ForwardIt end, BinaryPred p) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return beg;
		if(Tp.num_threads < 2) {
			return std::is_sorted_until(beg, end, p);

		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector < std::future<void> > futures(Tp.num_threads - 1);
		std::vector<std::pair<ForwardIt, int>> output(Tp.num_threads);
		ForwardIt block_start = beg;
		ForwardIt block_end = beg;
		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(is_sorted_until_block2<ForwardIt, BinaryPred>(),
										 block_start, (block_end + 1), p, std::ref(output[i]),
										 typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			is_sorted_until_block2<ForwardIt, BinaryPred>()(block_start, end, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}
		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(is_sorted_until_block2<ForwardIt, BinaryPred>(),
										block_start, (block_end + 1), p, std::ref(output[i]),
										typename std::iterator_traits<ForwardIt>::iterator_category());

				block_start = block_end;
			}
			is_sorted_until_block2<ForwardIt, BinaryPred>()(block_start, end, p,
					std::ref(output[Tp.num_threads - 1]),
					typename std::iterator_traits<ForwardIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

		auto val = std::find_if(output.begin(), output.end(),
								[](std::pair<ForwardIt,int> &v)->bool {return v.second==0;});
		return val->first;

	}
	/**
	 * handles each block of the reverse function.
	 */
	template<typename BidirIt>
	struct reverse_block {
			/**
			 *
			 * @param beg Bidirectional Iterator to the begining of the input container
			 * @param end Bidirectional Iterator to the end of the input container
			 * @param origBeg the original beginning of the container.
			 * @param origEnd the original ending of the container
			 * @param  used to make sure we at least have bidirectional iterators
			 */
			void operator ()(BidirIt beg, BidirIt end, BidirIt origBeg, BidirIt origEnd,
							 std::bidirectional_iterator_tag) {
				typename std::iterator_traits<BidirIt>::difference_type start = std::distance(
						origBeg,
						beg)
				+ 1;
				typename std::iterator_traits<BidirIt>::difference_type len = std::distance(beg,
																							end);
				typename std::iterator_traits<BidirIt>::difference_type i = 0;
				BidirIt end1 = origEnd;
				BidirIt swpEnd = end1 - start;

				for(i = 0; i < len; i++) {

					std::iter_swap(beg++, swpEnd--);

				}
			}
	};

	/**
	 *
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @return
	 */
	template<typename BidirIt, typename Tpolicy = LaunchPolicies<BidirIt> >
	void reverse(BidirIt beg, BidirIt end) {
		Tpolicy Tp;
		typename std::iterator_traits<BidirIt>::difference_type len = (end - beg) / 2; //the last added iterator is not included
		Tp.SetLaunchPolicies(len);
		if(!Tp.length)
			return;
		if(Tp.num_threads < 2) {
			std::reverse(beg, end);
			return;
		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<std::future<void>> futures(Tp.num_threads - 1);
		BidirIt block_start = beg;
		BidirIt block_end = beg;

		if(Tp.tTypes == ThreadTypes::standard) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				threads[i] = std::thread(reverse_block<BidirIt>(), block_start, block_end, beg, end,
										 typename std::iterator_traits<BidirIt>::iterator_category());

				block_start = block_end;
			}
			reverse_block<BidirIt>()(block_start, (beg + len), beg, end,
					typename std::iterator_traits<BidirIt>::iterator_category());
			std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		}

		if(Tp.tTypes == ThreadTypes::async) {
			for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

				std::advance(block_end, Tp.block_size);
				futures[i] = std::async(std::launch::async, reverse_block<BidirIt>(), block_start,
										block_end, beg, end,
										typename std::iterator_traits<BidirIt>::iterator_category());

				block_start = block_end;
			}
			reverse_block<BidirIt>()(block_start, (beg + len), beg, end,
					typename std::iterator_traits<BidirIt>::iterator_category());
			std::for_each(futures.begin(), futures.end(), std::mem_fn(&std::future<void>::wait));
		}

	}

	/*
	 * @param beg Iterator to the beginning of the input container
	 * @param end Iterator to the end of the input container
	 * @return
	 */
	template<typename BidirIt, typename Tpolicy = LaunchPolicies<BidirIt> >
	void reverse2(BidirIt beg, BidirIt end, unsigned int N = std::thread::hardware_concurrency()) {
		Tpolicy Tp;
		Tp.max_hardware_threads = N;
		typename std::iterator_traits<BidirIt>::difference_type len = (end - beg) / 2; //the last added iterator is not included
		Tp.SetLaunchPolicies(len);
		if(!Tp.length)
			return;
		if(Tp.num_threads < 2 or N < 2 or N == 0) {
			std::reverse(beg, end);
			return;
		}

		std::vector < std::thread > threads(Tp.num_threads - 1);
		BidirIt block_start = beg;
		BidirIt block_end = beg;
		for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			threads[i] = std::thread(reverse_block<BidirIt>(), block_start, block_end, beg, end,
									 typename std::iterator_traits<BidirIt>::iterator_category());

			block_start = block_end;
		}
		reverse_block<BidirIt>()(block_start, (beg + len), beg, end,
				typename std::iterator_traits<BidirIt>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));

	}

	//TODO:Implement swap_ranges...
	/**
	 * handles each block of the swap_ranges function.
	 */
	template<typename ForwardIt1, typename ForwardIt2>
	struct swap_ranges_block {
			/**
			 *
			 * @param beg1 Forward Iterator to the beginning of the first input container
			 * @param end1 Forward Iterator to the end of the first input container
			 * @param beg2 Forward Iterator to the beginning of the second container.
			 * @param  used to make sure we at least have bidirectional iterators
			 */
			void operator ()(ForwardIt1 beg1, ForwardIt1 end1, ForwardIt2 beg2, ForwardIt2 & ret,
							 std::forward_iterator_tag, std::forward_iterator_tag) {
				ret = std::swap_ranges(beg1, end1, beg2);
			}
	};
	/**
	 *
	 * @param beg1 Iterator to the beginning of the input container
	 * @param end1 Iterator to the end of the input container
	 * @param beg2 Iterator to the beginning of the second container.
	 * @return
	 */
	template<typename ForwardIt1, typename ForwardIt2, typename Tpolicy = LaunchPolicies<ForwardIt1> >
	ForwardIt2 swap_ranges(ForwardIt1 beg1, ForwardIt2 end1, ForwardIt2 beg2) {
		Tpolicy Tp;
		Tp.SetLaunchPolicies(beg1, end1);
		if(!Tp.length)
			return beg2;
		if(Tp.num_threads < 2) {
			return std::swap_ranges(beg1, end1, beg2);

		}
		std::vector < std::thread > threads(Tp.num_threads - 1);
		std::vector<ForwardIt2> output(Tp.num_threads);
		ForwardIt1 block_start = beg1;
		ForwardIt1 block_end = beg1;
		ForwardIt2 block_start2 = beg2;
		ForwardIt2 block_end2 = beg2;
		for(unsigned int i = 0; i < (Tp.num_threads - 1); i++) {

			std::advance(block_end, Tp.block_size);
			std::advance(block_end2, Tp.block_size);
			threads[i] = std::thread(swap_ranges_block<ForwardIt1, ForwardIt2>(), block_start,
									 block_end, block_start2, std::ref(output[i]),
									 typename std::iterator_traits<ForwardIt1>::iterator_category(),
									 typename std::iterator_traits<ForwardIt2>::iterator_category());

			block_start = block_end;
			block_start2 = block_end2;
		}
		swap_ranges_block<ForwardIt1, ForwardIt2>()(block_start, end1, block_start2,
				std::ref(output[Tp.num_threads]),
				typename std::iterator_traits<ForwardIt1>::iterator_category(),
				typename std::iterator_traits<ForwardIt2>::iterator_category());
		std::for_each(threads.begin(), threads.end(), std::mem_fn(&std::thread::join));
		return output[Tp.num_threads];

	}

	/**
	 * Insertion sort helper
	 */
	template<typename BiIt>
	struct insert_sort_block {
			/**
			 *
			 * @param beg iterator to the beginning of the input container
			 * @param end iterator to the end of the input container
			 * @param  make sure we at least have a bidirectional iterator
			 */
			void operator()(BiIt beg, BiIt end, std::bidirectional_iterator_tag) {
				BiIt first = beg;
				auto fmin = std::min_element(beg, end);
				std::iter_swap(fmin, first);
				while(++first < end) {
					for(auto v = first; *v < *(v - 1); v--)
						std::iter_swap(v, (v - 1));
				}
			}
	};

	/**
	 * Helper for insertion sort with binary predicate for comparison
	 */
	template<typename BiIt, typename BinaryPred>
	struct insert_sort_block2 {
			/**
			 *
			 * @param beg iterator to the beginning of the input container
			 * @param end iterator to the end of the input container
			 * @param p Binary predicate used for comparison.
			 * @param  make sure we at least have a bidirectional iterator
			 */
			void operator()(BiIt beg, BiIt end, BinaryPred p, std::bidirectional_iterator_tag) {
				BiIt first = beg;
				auto fmin = std::min_element(beg, end);
				std::iter_swap(fmin, first);
				while(++first < end) {
					for(auto v = first; p(*v, *(v - 1)); v--)
						std::iter_swap(v, (v - 1));
				}
			}
	};

	/**
	 *
	 * Beginning of building block for inplace parallel merge and parallel merge with copy used in parallel insertion sort.
	 * used to do a parallel swap of regions which is equivalent to a rotate
	 *
	 * @param beg Iterator to the beginning of the range to be rotated
	 * @param mid Iterator to the element we are rotating around
	 * @param end end of the range for which we are rotating
	 * @param N a parameter used to determine the number of threads used.
	 */
	template<typename BiDirIt, unsigned int blcksz = 1024,
			typename Tpolicy = LaunchPolicies<BiDirIt> >
	void inplace_par_swap_helper(BiDirIt beg, BiDirIt mid, BiDirIt end, unsigned int N,
								 std::bidirectional_iterator_tag) {
		Tpolicy Tp;

		auto len2 = std::distance(mid, end);
		auto beg2 = beg;

		Tp.max_hardware_threads = N;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return;
		if(Tp.num_threads < 2 or N < 2 or N == 0 or Tp.length < blcksz) {
			//perform swap
			std::reverse(beg, end);
			mid = std::next(beg2, len2);

			std::reverse(beg, mid);
			std::reverse(mid, end);

		}

		//LaunchPolicies<BiDirIt,blcksz,v> policy;
		parallel::reverse2<BiDirIt, Tpolicy>(beg, end, N);
		mid = std::next(beg2, len2);
		parallel::reverse2<BiDirIt, Tpolicy>(beg, mid, N);
		parallel::reverse2<BiDirIt, Tpolicy>(mid, end, N);
		return;

	}

	/**
	 *
	 * Beginning of building block for inplace parallel merge and parallel merge with copy used in parallel insertion sort.
	 * used to do a parallel swap of regions which is equivalent to a rotate
	 *
	 * @param beg Iterator to the beginning of the range to be rotated
	 * @param mid Iterator to the element we are rotating around
	 * @param end end of the range for which we are rotating
	 * @param N a parameter used to determine the number of threads used.
	 */
	template<typename BiDirIt, unsigned int blcksz = 1024,
			typename Tpolicy = LaunchPolicies<BiDirIt> >
	void inplace_par_swap(BiDirIt beg, BiDirIt mid, BiDirIt end, unsigned int N =
			std::thread::hardware_concurrency()) {
		parallel::inplace_par_swap_helper<BiDirIt, blcksz, Tpolicy>(beg, mid, end, N,
				typename std::iterator_traits<BiDirIt>::iterator_category());
	}
	/**
	 *
	 * Beginning of building block for inplace parallel merge and parallel merge with copy used in parallel insertion sort.
	 * used to do a parallel swap of regions which is equivalent to a rotate
	 *
	 * @param beg Iterator to the beginning of the range to be rotated
	 * @param mid Iterator to the element we are rotating around
	 * @param end end of the range for which we are rotating
	 * @param N a parameter used to determine the number of threads used.
	 */
	template<typename BiDirIt, unsigned int blcksz = 1024,
			typename Tpolicy = LaunchPolicies<BiDirIt> >
	void rotate(BiDirIt beg, BiDirIt n_first, BiDirIt end, unsigned int N =
			std::thread::hardware_concurrency()) {
		parallel::inplace_par_swap_helper<BiDirIt, blcksz, Tpolicy>(beg, n_first, end, N,
				typename std::iterator_traits<BiDirIt>::iterator_category());
	}
	/**
	 *
	 * Beginning of building block for inplace parallel merge and parallel merge with copy used in parallel insertion sort.
	 * used to do a parallel swap of regions which is equivalent to a rotate
	 *
	 * @param beg Iterator to the beginning of the range to be rotated
	 * @param mid Iterator to the element we are rotating around
	 * @param end end of the range for which we are rotating
	 * @param N a parameter used to determine the number of threads used.
	 */
	template<typename ForwardIt, unsigned int blcksz = 1024, typename Tpolicy = LaunchPolicies<
			ForwardIt> >
	void single_inplace_par_swap(ForwardIt beg, ForwardIt mid, ForwardIt end, unsigned int N) {
		Tpolicy Tp;

		auto len2 = std::distance(mid, end);
		auto beg2 = beg;

		Tp.max_hardware_threads = N;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return;

		//perform swap
		std::reverse(beg, end);
		mid = std::next(beg2, len2);
		std::reverse(beg, mid);
		std::reverse(mid, end);

	}

	/**
	 *
	 * @param begin
	 * @param end
	 * @param N
	 */
	//test code  parallel merge sort
	//using namespace std;
	template<typename Iter>
	void mergesort_mt1(Iter begin, Iter end, unsigned int N = std::thread::hardware_concurrency()) {
		auto len = std::distance(begin, end);
		if(len < 2)
			return;

		Iter mid = std::next(begin, len / 2);
		if(N > 1) {
			auto fn = std::async(std::launch::async, mergesort_mt1<Iter>, begin, mid, N - 2);
			mergesort_mt1(mid, end, N - 2);
			fn.wait();
		}
		else {
			mergesort_mt1(begin, mid, 0);
			mergesort_mt1(mid, end, 0);
		}

		std::inplace_merge(begin, mid, end);
	}

	template<typename Iter>
	void mergesort_mt2(Iter begin, Iter end, unsigned int N = std::thread::hardware_concurrency()) {
		auto len = std::distance(begin, end);
		if(len <= 1024) {
			std::stable_sort(begin, end);
			return;
		}

		Iter mid = std::next(begin, len / 2);
		if(N > 1) {
			auto fn = std::async(std::launch::async, mergesort_mt2<Iter>, begin, mid, N - 2);
			mergesort_mt2(mid, end, N - 2);
			fn.wait();
		}
		else {
			mergesort_mt2(begin, mid, 0);
			mergesort_mt2(mid, end, 0);
		}

		std::inplace_merge(begin, mid, end);
	}

	template<typename Iter>
	void mergesort_mt3(Iter begin, Iter end, unsigned int N = std::thread::hardware_concurrency()) {
		auto len = std::distance(begin, end);
		if(len <= 1024 || N < 2) {
			std::stable_sort(begin, end);
			return;
		}

		Iter mid = std::next(begin, len / 2);
		auto fn = std::async(std::launch::async, mergesort_mt3<Iter>, begin, mid, N - 2);
		mergesort_mt3(mid, end, N - 2);
		fn.wait();
		std::inplace_merge(begin, mid, end);
	}
	//standard co-ranks
	/**
	 * Function used to find the elements of ranks up to k in two sorted ranges
	 * @param i The range upto that we need (equivalent to finding n-th element of two arrays) kind of
	 * @param begA Iterator to the beginning of the first range
	 * @param endA Iterator to the end of the first range
	 * @param begB Iterator to the beginning of the second range
	 * @param endB Iterator to the end of the second range
	 * @return
	 */
	template<typename InputIt>
	std::pair<size_t, size_t> find_coranks(size_t i, InputIt begA, InputIt endA, InputIt begB,
			InputIt endB) {
		auto m = static_cast<size_t>(std::distance(begA, endA));
		auto n = static_cast<size_t>(std::distance(begB, endB));
		auto j = static_cast<size_t>(std::min(i, m));
		auto k = i - j;
		auto jlow = std::max(static_cast<size_t>(0), i - n);
		auto klow = k;
		auto delta = jlow;
		auto active = true;

		while(active) {

			if(j > 0 and k < n and *(begA + j - 1) > *(begB + k)) {
				delta = std::ceil((j - jlow) / 2);
				klow = k;
				j = j - delta;
				k = k + delta;
			}
			else if(k > 0 and j < m and *(begB + k - 1) >= *(begA + j)) {
				delta = std::ceil((k - klow) / 2);
				jlow = j;
				j = j + delta;
				k = k - delta;

			}
			else
				active = false;
		}
		return std::make_pair(j, k);
	}

	/**
	 * Function used to find the elements of ranks up to k in two sorted ranges
	 * @param i The range upto that we need (equivalent to finding n-th element of two arrays) kind of
	 * @param begA Iterator to the beginning of the first range
	 * @param endA Iterator to the end of the first range
	 * @param begB Iterator to the beginning of the second range
	 * @param endB Iterator to the end of the second range
	 * @param op The binary operator used for comparison
	 * @return
	 */
	template<typename InputIt, typename BinaryComp>
	std::pair<size_t, size_t> find_coranks(size_t i, InputIt begA, InputIt endA, InputIt begB,
			InputIt endB, BinaryComp op) {
		auto m = static_cast<size_t>(std::distance(begA, endA));
		auto n = static_cast<size_t>(std::distance(begB, endB));
		auto j = static_cast<size_t>(std::min(i, m));
		auto k = i - j;
		auto jlow = std::max(static_cast<size_t>(0), i - n);
		auto klow = k;
		auto delta = jlow;
		auto active = true;

		while(active) {

			if(j > 0 and k < n and op(*(begB + k), *(begA + j - 1))) {
				delta = std::ceil((j - jlow) / 2);
				klow = k;
				j = j - delta;
				k = k + delta;
			}
			else if(k > 0 and j < m and !(op(*(begB + k - 1), *(begA + j)))) {
				delta = std::ceil((k - klow) / 2);
				jlow = j;
				j = j + delta;
				k = k - delta;

			}
			else
				active = false;
		}
		return std::make_pair(j, k);
	}
	/**
	 * Actual Implementation of the parallel inplace merge
	 * @param beg Iterator to the beginning of the first  range
	 * @param mid Iterator to the end of the first range and beginning of the second range.
	 * @param end Iterator to the end of the second range.
	 * @param N parameter N used to determine how many threads to run.
	 * @param
	 */
	template<typename BiDirIt, typename Tpolicy = LaunchPolicies<BiDirIt>>
			void inplace_merge_helper(BiDirIt beg, BiDirIt mid, BiDirIt end, unsigned int N,
									  std::bidirectional_iterator_tag) {

		Tpolicy Tp;
		auto beg2 = beg;
		auto end2 = end;
		auto mid2 = mid;
		auto len1 = std::distance(beg, mid);

		Tp.max_hardware_threads = N;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return;
		if((N < 2) or (Tp.num_threads < 2) or (Tp.length < 2048)) {
			std::inplace_merge(beg, mid, end);
			return;
		}
		auto half = Tp.length / 2;
		auto rnks = find_coranks(half, beg, mid, mid, end);
		auto mid1 = std::next(beg2, half);
		beg2 = beg;

		auto start1 = std::next(beg2, rnks.first);
		auto end1 = std::next(mid2, rnks.second);

		inplace_par_swap<BiDirIt, 2048, Tpolicy>(start1, mid, end1, N);
		beg2 = beg;
		end2 = end;
		mid2 = mid;
		mid2 = std::next(beg2, rnks.first);
		beg2 = beg;
		mid1 = std::next(beg2, half);
		auto mid3 = std::next(beg2, half + (len1 - rnks.first));
		auto fn = std::async(std::launch::async, parallel::inplace_merge_helper<BiDirIt, Tpolicy>,
							 beg, mid2, mid1, N - 2,
							 typename std::iterator_traits<BiDirIt>::iterator_category());
		parallel::inplace_merge_helper<BiDirIt, Tpolicy>(mid1, mid3, end, N - 2,
				typename std::iterator_traits<BiDirIt>::iterator_category());
		fn.wait();

		return;
	}
	/**
	 * Actual Implementation of the parallel inplace merge with Binary Operator defined
	 * @param beg Iterator to the beginning of the first  range
	 * @param mid Iterator to the end of the first range and beginning of the second range.
	 * @param end Iterator to the end of the second range.
	 * @param op Binary operator used for comparison.
	 * @param N parameter N used to determine how many threads to run.
	 * @param
	 */
	template<typename BiDirIt, typename BinaryOp, typename Tpolicy = LaunchPolicies<BiDirIt>>
			void inplace_merge_helper(BiDirIt beg, BiDirIt mid, BiDirIt end, BinaryOp op, unsigned int N,
									  std::bidirectional_iterator_tag) {

		Tpolicy Tp;
		auto beg2 = beg;
		auto end2 = end;
		auto mid2 = mid;
		auto len1 = std::distance(beg, mid);

		Tp.max_hardware_threads = N;
		Tp.SetLaunchPolicies(beg, end);
		if(!Tp.length)
			return;
		if((N < 2) or (Tp.num_threads < 2) or (Tp.length < 2048)) {
			std::inplace_merge(beg, mid, end, op);
			return;
		}
		auto half = Tp.length / 2;
		auto rnks = find_coranks(half, beg, mid, mid, end, op);
		auto mid1 = std::next(beg2, half);
		beg2 = beg;

		auto start1 = std::next(beg2, rnks.first);
		auto end1 = std::next(mid2, rnks.second);

		inplace_par_swap<BiDirIt, 2048, Tpolicy>(start1, mid, end1, N);
		beg2 = beg;
		end2 = end;
		mid2 = mid;
		mid2 = std::next(beg2, rnks.first);
		beg2 = beg;
		mid1 = std::next(beg2, half);
		auto mid3 = std::next(beg2, half + (len1 - rnks.first));
		auto fn = std::async(std::launch::async,
							 parallel::inplace_merge_helper<BiDirIt, BinaryOp, Tpolicy>, beg, mid2, mid1, op,
							 N - 2, typename std::iterator_traits<BiDirIt>::iterator_category());
		parallel::inplace_merge_helper<BiDirIt, BinaryOp, Tpolicy>(mid1, mid3, end, op, N - 2,
				typename std::iterator_traits<BiDirIt>::iterator_category());
		fn.wait();

		return;
	}
	/**
	 * Actual inplace merge function identical to STL version
	 * @param beg Iterator to the beginning of the first sorted range
	 * @param mid Iterator to the end of the first sorted range and beginning of the second sorted range
	 * @param end Iterator to the end of the second sorted range
	 * @param N parameter determining the number of threads to use.
	 */
	template<typename BiDirIt, typename Tpolicy = LaunchPolicies<BiDirIt>>
			void inplace_merge(BiDirIt beg, BiDirIt mid, BiDirIt end, unsigned int N =
					std::thread::hardware_concurrency()) {
		;
		inplace_merge_helper<BiDirIt, Tpolicy>(beg, mid, end, N,
				typename std::iterator_traits<BiDirIt>::iterator_category());
	}
	/**
	 * Actual inplace merge function identical to STL version
	 * @param beg Iterator to the beginning of the first sorted range
	 * @param mid Iterator to the end of the first sorted range and beginning of the second sorted range
	 * @param end Iterator to the end of the second sorted range
	 * @param op Binary operator used for comparison.
	 * @param N parameter determining the number of threads to use.
	 */
	template<typename BiDirIt, typename BinaryOp, typename Tpolicy = LaunchPolicies<BiDirIt>>
			void inplace_merge(BiDirIt beg, BiDirIt mid, BiDirIt end, BinaryOp op, unsigned int N =
					std::thread::hardware_concurrency()) {
		;
		inplace_merge_helper<BiDirIt, BinaryOp, Tpolicy>(beg, mid, end, op, N,
				typename std::iterator_traits<BiDirIt>::iterator_category());
	}
	/**
	 * stable sort
	 * @param beg Iterator to the begining of the range
	 * @param en Iterator to the end of the range
	 * @param N determining how many threads to be run
	 * @param
	 */
	template<typename RanIt, typename Tpolicy = LaunchPolicies<RanIt>>
			void stable_sort_helper(RanIt beg, RanIt en, unsigned int N, std::random_access_iterator_tag) {

		auto len = std::distance(beg, en);
		if(len <= 8192 or N < 2) {
			std::stable_sort(beg, en);
			return;
		}

		auto mid = std::next(beg, len / 2);

		auto fn = std::async(std::launch::async, stable_sort_helper<RanIt, Tpolicy>, beg, mid,
							 N - 2, typename std::iterator_traits<RanIt>::iterator_category());
		stable_sort_helper<RanIt, Tpolicy>(mid, en, N - 2,
				typename std::iterator_traits<RanIt>::iterator_category());

		fn.wait();
		//std::cout<<N << " finished"<<std::endl;
		parallel::inplace_merge<RanIt, Tpolicy>(beg, mid, en, N);
	}
	/**
	 * stable sort
	 * @param beg Iterator to the begining of the range
	 * @param en Iterator to the end of the range
	 * @param op Binary operator used for comparison
	 * @param N determining how many threads to be run
	 * @param
	 */
	template<typename RanIt, typename BinaryOp, typename Tpolicy = LaunchPolicies<RanIt>>
			void stable_sort_helper(RanIt beg, RanIt en, BinaryOp op, unsigned int N,
									std::random_access_iterator_tag) {

		auto len = std::distance(beg, en);
		if(len <= 2048 or N < 2) {
			std::stable_sort(beg, en);
			return;
		}

		auto mid = std::next(beg, len / 2);

		auto fn = std::async(std::launch::async, stable_sort_helper<RanIt, BinaryOp, Tpolicy>, beg,
							 mid, op, N - 2, std::iterator_traits<RanIt>::iterator_category());
		stable_sort_helper<RanIt, BinaryOp, Tpolicy>(mid, en, N - 2,
				std::iterator_traits<RanIt>::iterator_category());

		fn.wait();
		//std::cout<<N << " finished"<<std::endl;
		parallel::inplace_merge<RanIt, BinaryOp, Tpolicy>(beg, mid, en, op, N);
	}
	/**
	 * Actual stable sort call
	 * @param beg Iterator to the beginning of the range to sort
	 * @param en Iterator to the end of the range to sort
	 * @param N parameter determining how many threads to be run
	 */
	template<typename RandomIt, typename Tpolicy = LaunchPolicies<RandomIt>>
			void stable_sort(RandomIt beg, RandomIt en, unsigned int N =
					std::thread::hardware_concurrency()) {
		stable_sort_helper<RandomIt, Tpolicy>(beg, en, N,
				typename std::iterator_traits<RandomIt>::iterator_category());
	}
	/**
	 * Actual stable sort call
	 * @param beg Iterator to the beginning of the range to sort
	 * @param en Iterator to the end of the range to sort
	 * @param op Binary operator used for comparison
	 * @param N parameter determining how many threads to be run
	 */
	template<typename RandomIt, typename BinaryOp, typename Tpolicy = LaunchPolicies<RandomIt>>
			void stable_sort(RandomIt beg, RandomIt en, BinaryOp op, unsigned int N =
					std::thread::hardware_concurrency()) {
		stable_sort_helper<RandomIt, BinaryOp, Tpolicy>(beg, en, op, N,
				typename std::iterator_traits<RandomIt>::iterator_category());
	}
	/**
	 * helper function for stable merge , merging two ranges into one
	 * @param be1 Iterator to beginning of first input range
	 * @param en1 Iterator to the end of first input range
	 * @param be2 Iterator to the beginning of second sorted input range
	 * @param en2 Iterator to the end of the second sorted input range
	 * @param start1 Iterator to the beginning of the output range
	 * @param N parameter N determining the number of threads to use
	 * @param
	 * @param
	 * @param
	 */
	template<typename InputIt1, typename InputIt2, typename OutputIt,
	typename Tpolicy = LaunchPolicies<InputIt1>>
	void merge_helper(InputIt1 be1, InputIt1 en1, InputIt2 be2, InputIt2 en2, OutputIt start1,
					  unsigned int N, std::input_iterator_tag, std::input_iterator_tag,
					  std::output_iterator_tag) {

		Tpolicy Tp;
		auto beg11 = be1;
		auto beg22 = be2;
		auto start11 = start1;
		auto len = std::distance(be1, en1) + std::distance(be2, en2);

		Tp.max_hardware_threads = N;
		Tp.SetLaunchPolicies(len, N, Tp.block_size);
		if(!Tp.length)
			return;
		if((N < 2) or (Tp.num_threads < 2) or (Tp.length < 2048)) {
			std::merge(be1, en1, be2, en2, start1);
			return;
		}
		auto half = Tp.length / 2;
		auto rnks = find_coranks(half, be1, en1, be2, en2);
		//auto mid1 = std::next(be2, half);
		//beg2 = beg;

		auto start111 = std::next(start1, rnks.first + rnks.second);
		auto end111 = std::next(beg11, rnks.first);
		auto end222 = std::next(beg22, rnks.second);

		auto fn = std::async(std::launch::async,
							 parallel::merge_helper<InputIt1, InputIt2, OutputIt, Tpolicy>, be1, end111, be2,
							 end222, start1, N - 2, typename std::iterator_traits<InputIt1>::iterator_category(),
							 typename std::iterator_traits<InputIt2>::iterator_category(),
							 typename std::iterator_traits<OutputIt>::iterator_category());
		parallel::merge_helper<InputIt1, InputIt2, OutputIt, Tpolicy>(end111, en1, end222, en2,
				start111, N - 2, typename std::iterator_traits<InputIt1>::iterator_category(),
				typename std::iterator_traits<InputIt2>::iterator_category(),
				typename std::iterator_traits<OutputIt>::iterator_category());
		fn.wait();

		return;
	}

	/**
	 * helper function for stable merge , merging two ranges into one
	 * @param be1 Iterator to beginning of first input range
	 * @param en1 Iterator to the end of first input range
	 * @param be2 Iterator to the beginning of second sorted input range
	 * @param en2 Iterator to the end of the second sorted input range
	 * @param start1 Iterator to the beginning of the output range
	 * @param op Binary operator used for comparison
	 * @param N parameter N determining the number of threads to use
	 * @param
	 * @param
	 * @param
	 */
	template<typename InputIt1, typename InputIt2, typename OutputIt, typename BinaryOp,
	typename Tpolicy = LaunchPolicies<InputIt1>>
	void merge_helper(InputIt1 be1, InputIt1 en1, InputIt2 be2, InputIt2 en2, OutputIt start1,
					  BinaryOp op, unsigned int N, std::input_iterator_tag,
					  std::input_iterator_tag, std::output_iterator_tag) {

		Tpolicy Tp;
		auto beg11 = be1;
		auto beg22 = be2;
		auto start11 = start1;
		auto len = std::distance(be1, en1) + std::distance(be2, en2);

		Tp.max_hardware_threads = N;
		Tp.SetLaunchPolicies(len, N, Tp.block_size);
		if(!Tp.length)
			return;
		if((N < 2) or (Tp.num_threads < 2) or (Tp.length < 2048)) {
			std::merge(be1, en1, be2, en2, start1);
			return;
		}
		auto half = Tp.length / 2;
		auto rnks = find_coranks(half, be1, en1, be2, en2, op);
		//auto mid1 = std::next(be2, half);
		//beg2 = beg;

		auto start111 = std::next(start1, rnks.first + rnks.second);
		auto end111 = std::next(beg11, rnks.first);
		auto end222 = std::next(beg22, rnks.second);

		auto fn = std::async(std::launch::async,
							 parallel::merge_helper<InputIt1, InputIt2, OutputIt, BinaryOp, Tpolicy>, be1,
							 end111, be2, end222, start1, op, N - 2,
							 typename std::iterator_traits<InputIt1>::iterator_category(),
							 typename std::iterator_traits<InputIt2>::iterator_category(),
							 typename std::iterator_traits<OutputIt>::iterator_category());
		parallel::merge_helper<InputIt1, InputIt2, OutputIt, BinaryOp, Tpolicy>(end111, en1, end222,
				en2, start111, op, N - 2,
				typename std::iterator_traits<InputIt1>::iterator_category(),
				typename std::iterator_traits<InputIt2>::iterator_category(),
				typename std::iterator_traits<OutputIt>::iterator_category());
		fn.wait();

		return;
	}

	/**
	 *  stable merge , merging two ranges into one
	 * @param be1 Iterator to beginning of first input range
	 * @param en1 Iterator to the end of first input range
	 * @param be2 Iterator to the beginning of second sorted input range
	 * @param en2 Iterator to the end of the second sorted input range
	 * @param start1 Iterator to the beginning of the output range
	 * @param N parameter N determining the number of threads to use
	 */
	template<typename InputIt1, typename InputIt2, typename OutputIt,
	typename Tpolicy = LaunchPolicies<InputIt1>>
	void merge(InputIt1 be1, InputIt1 en1, InputIt2 be2, InputIt2 en2, OutputIt start1,
			   unsigned int N = std::thread::hardware_concurrency()) {
		parallel::merge_helper<InputIt1, InputIt2, OutputIt, Tpolicy>(be1, en1, be2, en2, start1, N,
				std::iterator_traits<InputIt1>::iterator_category(),
				std::iterator_traits<InputIt2>::iterator_category(),
				std::iterator_traits<OutputIt>::iterator_category());
	}

	/**
	 *  stable merge , merging two ranges into one
	 * @param be1 Iterator to beginning of first input range
	 * @param en1 Iterator to the end of first input range
	 * @param be2 Iterator to the beginning of second sorted input range
	 * @param en2 Iterator to the end of the second sorted input range
	 * @param start1 Iterator to the beginning of the output range
	 * @param op Binary operator used for comparison
	 * @param N parameter N determining the number of threads to use
	 *
	 */
	template<typename InputIt1, typename InputIt2, typename OutputIt, typename BinaryOp,
	typename Tpolicy = LaunchPolicies<InputIt1>>
	void merge(InputIt1 be1, InputIt1 en1, InputIt2 be2, InputIt2 en2, OutputIt start1, BinaryOp op,
			   unsigned int N = std::thread::hardware_concurrency()) {
		parallel::merge_helper<InputIt1, InputIt2, OutputIt, BinaryOp, Tpolicy>(be1, en1, be2, en2,
				start1, op, N, std::iterator_traits<InputIt1>::iterator_category(),
				std::iterator_traits<InputIt2>::iterator_category(),
				std::iterator_traits<OutputIt>::iterator_category());
	}
	/**
	 *  stable partition algorithm
	 * @param beg Iterator to the beginning of the range
	 * @param end Iterator to the end of the range
	 * @param p Unary Predicate that represents the partion @see STL
	 * @param N Numbef of threads
	 * @param
	 * @return Iterator to the beginning of the block where the predicate is false
	 */
	template<typename BiDirIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<BiDirIt>>
			BiDirIt stable_partition_helper(BiDirIt beg, BiDirIt end, UnaryPredicate p, unsigned int N,
											std::bidirectional_iterator_tag) {

		Tpolicy Tp;
		auto beg2 = beg;
		auto end2 = end;

		Tp.max_hardware_threads = N;
		Tp.SetLaunchPolicies(beg, end, N);
		if(!Tp.length)
			return beg;
		if((N < 2) or (Tp.num_threads < 2) or (Tp.length < 2048)) {
			return std::stable_partition(beg, end, p);

		}
		auto half = Tp.length / 2;
		auto mid1 = std::next(beg2, half);

		auto first = std::async(std::launch::async,
								parallel::stable_partition_helper<BiDirIt, UnaryPredicate, Tpolicy>, beg, mid1, p,
								N - 2, typename std::iterator_traits<BiDirIt>::iterator_category());
		auto second = parallel::stable_partition_helper<BiDirIt, UnaryPredicate, Tpolicy>(mid1, end,
				p, N - 2, typename std::iterator_traits<BiDirIt>::iterator_category());
		first.wait();
		auto mid11 = first.get();
		auto mid22 = second.get();
		auto len11 = std::distance(beg, mid11);
		auto len22 = std::distance(mid1, mid22);
		parallel::rotate<BiDirIt, Tpolicy>(mid11, mid1, mid22, N); //swap regions

		return std::next(beg, len11 + len22);
	}

	/**
	 *   partition algorithm
	 * @param beg Iterator to the beginning of the range
	 * @param end Iterator to the end of the range
	 * @param p Unary Predicate that represents the partion @see STL
	 * @param N Numbef of threads
	 * @param
	 * @return Iterator to the beginning of the block where the predicate is false
	 */
	template<typename BiDirIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<BiDirIt>>
			BiDirIt partition_helper(BiDirIt beg, BiDirIt end, UnaryPredicate p, unsigned int N,
									 std::bidirectional_iterator_tag) {

		Tpolicy Tp;
		auto beg2 = beg;
		auto end2 = end;

		Tp.max_hardware_threads = N;
		Tp.SetLaunchPolicies(beg, end, N);
		if(!Tp.length)
			return beg;
		if((N < 2) or (Tp.num_threads < 2) or (Tp.length < 2048)) {
			return std::partition(beg, end, p);

		}
		auto half = Tp.length / 2;
		auto mid1 = std::next(beg2, half);

		auto first = std::async(std::launch::async,
								parallel::stable_partition_helper<BiDirIt, UnaryPredicate, Tpolicy>, beg, mid1, p,
								N - 2, typename std::iterator_traits<BiDirIt>::iterator_category());
		auto second = parallel::stable_partition_helper<BiDirIt, UnaryPredicate, Tpolicy>(mid1, end,
				p, N - 2, typename std::iterator_traits<BiDirIt>::iterator_category());
		first.wait();
		auto mid11 = first.get();
		auto mid22 = second.get();
		auto len11 = std::distance(beg, mid11);
		auto len22 = std::distance(mid1, mid22);
		parallel::rotate<BiDirIt, Tpolicy>(mid11, mid1, mid22, N); //swap regions

		return std::next(beg, len11 + len22);
	}

	/**
	 *  actual partition algorithm
	 * @param beg Iterator to the beginning of the range
	 * @param end Iterator to the end of the range
	 * @param p Unary Predicate that represents the partion @see STL
	 * @param N Numbef of threads
	 * @param
	 * @return Iterator to the beginning of the block where the predicate is false
	 */
	template<typename BiDirIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<BiDirIt>>
			BiDirIt stable_partition(BiDirIt beg, BiDirIt end, UnaryPredicate p, unsigned int N=std::thread::hardware_concurrency()) {
		parallel::stable_partition_helper<BiDirIt,UnaryPredicate,Tpolicy>(beg,end,p,N,typename std::iterator_traits<BiDirIt>::iterator_category());
	}

	/**
	 *  actual stable partition algorithm
	 * @param beg Iterator to the beginning of the range
	 * @param end Iterator to the end of the range
	 * @param p Unary Predicate that represents the partion @see STL
	 * @param N Numbef of threads
	 * @param
	 * @return Iterator to the beginning of the block where the predicate is false
	 */
	template<typename BiDirIt, typename UnaryPredicate, typename Tpolicy = LaunchPolicies<BiDirIt>>
			BiDirIt partition(BiDirIt beg, BiDirIt end, UnaryPredicate p, unsigned int N=std::thread::hardware_concurrency()) {
		parallel::partition_helper<BiDirIt,UnaryPredicate,Tpolicy>(beg,end,p,N,typename std::iterator_traits<BiDirIt>::iterator_category());
	}

}

#endif /* PARLIB_H_ */
