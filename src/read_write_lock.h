/* MIT License
 *
 * Copyright (c) 2018 Will Zhang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#pragma once

#include <mutex>
#include <condition_variable>

#ifdef USE_C_PTHREAD
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <sched.h>
#include <pthread.h>
#endif

namespace cpputil
{

class RWLock
{
#ifdef USE_C_PTHREAD
public:
    RWLock()
    {
        status_ = 0;
        waiting_readers_ = 0;
        waiting_writers_ = 0;
        pthread_mutex_init(&mtx_, NULL);
        pthread_cond_init(&read_cv_, NULL);
        pthread_cond_init(&write_cv_, NULL);
    }

	void ReadLock()
	{
        pthread_mutex_lock(&mtx_);
		waiting_readers_ += 1;
        int ret = 0;
        while (!(waiting_writers_ == 0 && status_ >= 0) && ret == 0) {
            ret = pthread_cond_wait(&read_cv_, &mtx_);
        }
		waiting_readers_ -= 1;
		status_ += 1;
        pthread_mutex_unlock(&mtx_);
	}

	void WriteLock()
	{
        pthread_mutex_lock(&mtx_);
		waiting_writers_ += 1;
        int ret = 0;
        while (!(status_ == 0) && ret == 0) {
            ret = pthread_cond_wait(&write_cv_, &mtx_);
        }
		waiting_writers_ -= 1;
		status_ = -1;
        pthread_mutex_unlock(&mtx_);
	}

	void UnLock()
	{

        pthread_mutex_lock(&mtx_);
		if (status_ == -1) {
			status_ = 0;
		}
		else
		{
			status_ -= 1;
		}
		if (waiting_writers_ > 0)
		{
			if (status_ == 0)
			{
                pthread_cond_signal(&write_cv_);
			}
		}
		else
		{
            pthread_cond_broadcast(&read_cv_);
		}
        pthread_mutex_unlock(&mtx_);
	}

private:
	/** status of the lock
	 * -1    : one writer
	 * 0     : no reader and no writer
	 * n > 0 : n reader
	 */
	int32_t status_;
	int32_t waiting_readers_;
	int32_t waiting_writers_;
	pthread_mutex_t mtx_;
	pthread_cond_t read_cv_;
	pthread_cond_t write_cv_;

#else
 public:
	RWLock() : status_(0), waiting_readers_(0), waiting_writers_(0) {}
	RWLock(const RWLock&) = delete;
	RWLock(RWLock&&) = delete;
	RWLock& operator = (const RWLock&) = delete;
	RWLock& operator = (RWLock&&) = delete;

	void ReadLock()
	{
		std::unique_lock<std::mutex> lck(mtx_);
		waiting_readers_ += 1;
		read_cv_.wait(lck, [&]() { return waiting_writers_ == 0 && status_ >= 0; });
		waiting_readers_ -= 1;
		status_ += 1;
	}

	void WriteLock()
	{
		std::unique_lock<std::mutex> lck(mtx_);
		waiting_writers_ += 1;
		write_cv_.wait(lck, [&]() { return status_ == 0; });
		waiting_writers_ -= 1;
		status_ = -1;
	}

	void UnLock()
	{

		std::unique_lock<std::mutex> lck(mtx_);
		if (status_ == -1) {
			status_ = 0;
		} 
		else
		{
			status_ -= 1;
		}
		if (waiting_writers_ > 0)
		{
			if (status_ == 0)
			{
				write_cv_.notify_one();
			}
		} 
		else
		{
			read_cv_.notify_all();
		}
	}


 private:
	/** status of the lock
	 * -1    : one writer
	 * 0     : no reader and no writer
	 * n > 0 : n reader
	 */
	int32_t status_;
	int32_t waiting_readers_;
	int32_t waiting_writers_;
	std::mutex mtx_;
	std::condition_variable read_cv_;
	std::condition_variable write_cv_;

#endif
};

}  // namespace cpputil
