#pragma once

#include "miner_work.hpp"
#include "environment.hpp"
#include "console.hpp"
#include "pool_data.hpp"
#include "read_write_lock.h"

#include <atomic>

namespace xmrstak
{

struct globalStates
{
	static inline globalStates& inst()
	{
		auto& env = environment::inst();
		if(env.pglobalStates == nullptr)
			env.pglobalStates = new globalStates;
		return *env.pglobalStates;
	}

	//pool_data is in-out winapi style
	void switch_work(miner_work& pWork, pool_data& dat);

	inline void calc_start_nonce(uint32_t& nonce, bool use_nicehash, uint32_t reserve_count)
	{
		if(use_nicehash)
			nonce = (nonce & 0xFF000000) | iGlobalNonce.fetch_add(reserve_count);
		else
			nonce = iGlobalNonce.fetch_add(reserve_count);
	}

	void consume_work( miner_work& threadWork, uint64_t& currentJobId);

	miner_work oGlobalWork;
	uint64_t iThreadCount;
	std::atomic<uint64_t> iGlobalJobNo;
	std::atomic<uint64_t> iConsumeCnt;
	std::atomic<uint32_t> iGlobalNonce;
	size_t pool_id = invalid_pool_id;

private:
	globalStates() : iThreadCount(0), iGlobalJobNo(0), iConsumeCnt(0)
	{
	}

	::cpputil::RWLock jobLock;
};

} // namespace xmrstak
