/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <ghoul/misc/threadpool.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/defer.h>
#include <chrono>
#include <exception>
#include <utility>

namespace {
    // The time-out time for the condition_variable inside the worker threads
    constexpr std::chrono::seconds WaitTime(1);
} // namespace

namespace ghoul {

ThreadPool::ThreadPool(int nThreads, std::function<void()> workerInit,
                       std::function<void()> workerDeinit,
                       thread::ThreadPriorityClass tpc, thread::ThreadPriorityLevel tpl,
                       thread::Background bg)
    : _workers(nThreads)
    , _taskQueue(std::make_shared<TaskQueue>())
    , _isRunning(std::make_shared<std::atomic_bool>(true))
    , _nWaiting(std::make_shared<std::atomic_int>(0))
    , _mutex(std::make_shared<std::mutex>())
    , _cv(std::make_shared<std::condition_variable>())
    , _workerInitialization(std::move(workerInit))
    , _workerDeinitialization(std::move(workerDeinit))
    , _threadPriorityClass(tpc)
    , _threadPriorityLevel(tpl)
    , _threadBackground(bg)
{
    ghoul_assert(nThreads > 0, "nThreads must be bigger than 0");
    ghoul_assert(_workerInitialization, "workerInit must not be empty");
    ghoul_assert(_workerDeinitialization, "workerDeinit must not be empty");

    // Activate the workers
    for (Worker& w : _workers) {
        activateWorker(w);
    }

    ghoul_assert(isRunning(), "ThreadPool is not running");
}

ThreadPool::~ThreadPool() {
    if (isRunning()) {
        // The stop method cannot guarantee to be noexcept, so we have to catch potential
        // exceptions that might occur there. Though any exceptions will likely be coming
        // from either the detaching joining of threads
        try {
            // We have to wait for the remaining tasks and cannot detach as the lambda
            // expressions for the Workers have a copy of 'this'. So if we don't exist
            // anymore when they are destroyed, it is a problem
            stop(RunRemainingTasks::Yes, DetachThreads::No);
        }
        catch (std::exception& e) {
            LERRORC("ThreadPool", e.what());
        }
    }
}

void ThreadPool::start() {
    ghoul_assert(!isRunning(), "ThreadPool must not be running");

    *_isRunning = true;

    for (Worker& w : _workers) {
        activateWorker(w);
    }

    ghoul_assert(isRunning(), "ThreadPool is not running");
}

void ThreadPool::stop(RunRemainingTasks runTasks, DetachThreads detachThreads) {
    ghoul_assert(isRunning(), "ThreadPool must be running");
    ghoul_assert(
        !(runTasks && detachThreads),
        "Cannot run remaining tasks and detach threads"
    );

    // If we don't want to complete the remaining tasks, we'll get rid of them here
    if (!runTasks) {
        clearRemainingTasks();
    }

    // We first have to set '_isRunning' to false before waking up all threads as they
    // otherwise might go to sleep immediately again
    *_isRunning = false;

    // Wake up all of the threads, all of the threads that cannot find tasks will
    // terminate
    for (Worker& w : _workers) {
        _cv->notify_all();
        if (detachThreads) {
            // Detaching the thread to let it finish it's work independently
            w.thread->detach();
        }
        else {
            // Block until the thread is finished
            w.thread->join();
        }
    }

    // Delete all the workers. We don't want to actually delete them as we would otherwise
    // lose information about their sizes
    for (Worker& w : _workers) {
        w = { .thread = nullptr, .shouldTerminate = nullptr };
    }

    ghoul_assert(!isRunning(), "The ThreadPool is still running");
}

bool ThreadPool::isRunning() const {
    return *_isRunning;
}

void ThreadPool::resize(int nThreads) {
    ghoul_assert(nThreads > 0, "nThreads must be bigger than 0");

    const int oldNThreads = size();
    if (oldNThreads <= nThreads) {
        // If the number of threads has increased
        _workers.resize(nThreads);

        // We only want to activate the new workers if we are not currently running
        if (*_isRunning) {
            for (int i = oldNThreads; i < nThreads; i++) {
                activateWorker(_workers[i]);
            }
        }
    }
    else {
        // The number of threads has decreased
        for (int i = oldNThreads - 1; i >= nThreads; --i) {
            // Tell the superfluous threads to finish
            *(_workers[i].shouldTerminate) = true;

            // And detach the thread so we can safely remove the Worker object
            _workers[i].thread->detach();
        }
        // The notification will do nothing for the first 'nThreads' threads, but it will
        // cause the remaining 'nThreads - oldNThreads' to return
        _cv->notify_all();

        // Safe to delete because the threads are detached
        _workers.resize(nThreads);
    }
    ghoul_assert(size() == nThreads, "The ThreadPool contains wrong number of workers");
}

int ThreadPool::size() const {
    return static_cast<int>(_workers.size());
}

int ThreadPool::idleThreads() const {
    return *_nWaiting;
}

int ThreadPool::remainingTasks() const {
    return _taskQueue->size();
}

void ThreadPool::clearRemainingTasks() {
    while (!_taskQueue->isEmpty()) {
        _taskQueue->pop();
    }

    ghoul_assert(_taskQueue->isEmpty(), "Task queue is not empty");
}

void ThreadPool::activateWorker(Worker& worker) {
    // A copy of the shared ptr to the flag
    auto shouldTerminate = std::make_shared<std::atomic_bool>(false);

    // We create local copies of the important variables so that we are guaranteed that
    // they continue to exist when we pass them to the 'workerLoop' lamdba. Otherwise, the
    // ThreadPool might be destructed before the workers have finished (for example when
    // they are detached) and would then access already freed memory
    const std::shared_ptr<std::atomic_bool>& threadPoolIsRunning = _isRunning;
    const std::shared_ptr<std::atomic_int>& nWaiting = _nWaiting;
    const std::shared_ptr<TaskQueue>& taskQueue = _taskQueue;
    const std::shared_ptr<std::mutex>& mutex = _mutex;
    const std::shared_ptr<std::condition_variable>& cv = _cv;

    const std::function<void()>& workerInitialization = _workerInitialization;
    std::function<void()> workerDeinitialization = _workerDeinitialization;


    volatile bool finishedInitializing = false;

    // Capturing the shared_ptrs by value to maintain a copy
    auto workerLoop = [
        shouldTerminate, threadPoolIsRunning, &finishedInitializing, nWaiting, taskQueue,
        mutex, cv, workerInitialization, workerDeinitialization
    ]() {
        // Invoke the user-defined initialization function
        workerInitialization();
        // And invoke the user-defined deinitialization function when the scope is exited
        defer { workerDeinitialization(); };

        std::function<void()> task;
        bool hasTask = false;
        std::tie(task, hasTask) = taskQueue->pop();

        // Infinite look that only gets broken if this thread should terminate or if it
        // gets woken up without there being a task
        while (true) {
            // If there is something in the queue
            while (hasTask) {
                finishedInitializing = true;

                // Do the task
                task();

                // We cannot check for shouldTerminate earlier as if hasTask is true, we
                // have already retrieved that value from the stack and if we don't work
                // on it, it would disappear
                if (*shouldTerminate) {
                    return;
                }

                // If we shouldn't terminate, we can check if there is more work if there
                // is, we stay in this inner loop until there is no more work to be done
                std::tie(task, hasTask) = taskQueue->pop();
            }

            // If the ThreadPool has stopped running and there are no more tasks, we don't
            // need to sleep first, but can return immediately
            if (!*threadPoolIsRunning) {
                finishedInitializing = true;
                return;
            }

            // If we get here, there is no more work to be done and the ThreadPool is
            // still running, so we can sleep until there is more work
            (*nWaiting)++;
            while (true) {
                finishedInitializing = true;

                // We are doing this in an infinite loop, as we want to check regularly if
                // there is more work. This shouldn't be necessary in normal cases, but is
                // more of a last resort protection
                std::unique_lock lock(*mutex);
                cv->wait_for(lock, WaitTime);

                // We woke up, so either there is work to be done
                std::tie(task, hasTask) = taskQueue->pop();
                if (hasTask) {
                    (*nWaiting)--;
                    // We have a task now, so if we break we start over with loop #1 and
                    // do the work as we enter loop #2
                    break;
                }

                // Or we were asked to terminate or the ThreadPool is finished
                if (*shouldTerminate || !*threadPoolIsRunning) {
                    (*nWaiting)--;
                    return;
                }

                // If we reach this place, we just woke up spuriously or due to the timer
                // so we stay in loop #3 and go to sleep again
            }
        }
    };

    // We create the thread running our worker loop. It will start immediately, but that
    // is not a problem
    auto thread = std::make_unique<std::thread>(workerLoop);

    // Set the thread priority to the desired class and level
    thread::setPriority(*thread, _threadPriorityClass, _threadPriorityLevel);

    // And move the thread to a background priority if desired and supported
    if (_threadBackground == thread::Background::Yes) {
        thread::setThreadBackground(*thread, thread::Background::Yes);
    }

    // Overwrite the worker and we are done
    worker = {
        .thread = std::move(thread),
        .shouldTerminate = std::move(shouldTerminate)
    };

    while (!finishedInitializing) {}
}

std::tuple<ThreadPool::Task, bool> ThreadPool::TaskQueue::pop() {
    const std::unique_lock lock(_queueMutex);
    if (_queue.empty()) {
        // No work to be done, the default constructed Task is never read
        return std::tuple(Task(), false);
    }
    else {
        // We have a task, so we move it out of the queue
        Task t = std::move(_queue.front());
        // And remove the item
        _queue.pop();
        // And return the task together with a positive reply
        return std::tuple(std::move(t), true);
    }
}

void ThreadPool::TaskQueue::push(const ThreadPool::Task& task) {
    const std::unique_lock lock(_queueMutex);
    _queue.push(task);
}

bool ThreadPool::TaskQueue::isEmpty() const {
    const std::unique_lock lock(_queueMutex);
    return _queue.empty();
}

int ThreadPool::TaskQueue::size() const {
    const std::unique_lock lock(_queueMutex);
    return static_cast<int>(_queue.size());
}

} // namespace ghoul
