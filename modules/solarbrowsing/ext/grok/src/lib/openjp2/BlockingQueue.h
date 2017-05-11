#pragma once

#include <mutex>
#include <condition_variable>
#include <queue>
#include <vector>

template<typename Data> class BlockingQueue
{
public:
	BlockingQueue() : _active(true) {}
	void deactivate() {
		std::lock_guard<std::mutex> lk(_mutex);
		_active = false;
		_condition.notify_all();
	}

	void clear() {
		std::lock_guard<std::mutex> lk(_mutex);
		while (_queue.size() != 0)
			_queue.pop();
	}

	void push_no_lock(std::vector<Data>* data) {
		for (auto d = data->begin(); d != data->end(); ++d)
			_queue.push(*d);
	}

	void push(Data const& data)
	{
		{
			std::lock_guard<std::mutex> lk(_mutex);
			if (!_active)
				return;
			_queue.push(data);
		}
		_condition.notify_one();
	}

	bool empty() const
	{
		std::lock_guard<std::mutex> lk(_mutex);
		return _queue.empty();
	}

	bool tryPop(Data& value)
	{
		std::lock_guard<std::mutex> lk(_mutex);
		return pop(value);
	}

	bool waitAndPop(Data& value)
	{
		std::unique_lock<std::mutex> lk(_mutex);
		_condition.wait(lk, [this]{ return !_active || !_queue.empty(); });
		return pop(value);
	}

	size_t size() const {
		std::lock_guard<std::mutex> lk(_mutex);
		return _queue.size();
	}


private:

	bool pop(Data& value) {
		if (_queue.empty())
			return false;
		value = _queue.front();
		_queue.pop();
		return true;
	}

	std::queue<Data> _queue;
	mutable std::mutex _mutex;
	std::condition_variable _condition;
	bool _active;
};

