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
 *****************************************************************************************
 * Based on the CPTL implementation by Vitaliy Vitsentiy found here:                     *
 * https://github.com/vit-vit/CTPL                                                       *
 *****************************************************************************************
 *  Copyright (C) 2014 by Vitaliy Vitsentiy                                              *
 *                                                                                       *
 *  Licensed under the Apache License, Version 2.0 (the "License");                      *
 *  you may not use this file except in compliance with the License.                     *
 *  You may obtain a copy of the License at                                              *
 *                                                                                       *
 *     http://www.apache.org/licenses/LICENSE-2.0                                        *
 *                                                                                       *
 *  Unless required by applicable law or agreed to in writing, software                  *
 *  distributed under the License is distributed on an "AS IS" BASIS,                    *
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.             *
 *  See the License for the specific language governing permissions and                  *
 *  limitations under the License.                                                       *
 ****************************************************************************************/

#ifndef __GHOUL___THREADPOOL___H__
#define __GHOUL___THREADPOOL___H__

#include <ghoul/misc/boolean.h>
#include <ghoul/misc/thread.h>
#include <atomic>
#include <condition_variable>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include <queue>
#include <thread>
#include <tuple>
#include <vector>

namespace ghoul {

/**
 * The ThreadPool is a class that manages a list of threads (= ThreadPool::Worker%s) that
 * will perform tasks from a list. A ThreadPool is created with a specific number of
 * threads but can be #resize%d after the fact, which will change the number of active
 * threads managed by this ThreadPool. Tasks can be queued by the #queue function, which
 * returns a `std::future` object that contains the possible return value of the passed
 * task.
 *
 * Example use-case:
 * ```
 *  ghoul::ThreadPool pool(2);
 *
 * {
 *     std::future<int> ret = pool.queue([](){ return 1337; });
 *     auto urn = pool.queue([](){ return "foobar"; });
 *     ghoul_assert(ret.get() == 1337);
 *     ghoul_assert(urn.get() == "foobar");
 *  }
 *
 *  {
 *     auto f = [](int i, float f, std::string s) -> std::tuple<std::string, float, int> {
 *        return std::tuple(s, f, i);
 *     };
 *
 *     std::future<std::tuple<std::string, float, int>> r = pool.queue(f, 1, 2.f, "3");
 *     std::tuple<std::string, float, int> val = r.get();
 *     ghoul_assert("3" == std::get<0>(val));
 *     ghoul_assert(2.f == std::get<1>(val));
 *     ghoul_assert(1 == std::get<2>(val));
 *  }
 * ```
 *
 * Tasks passed to the ThreadPool as started in order a strict FIFO ordering.
 *
 * Workers can be initialized with custom functions that are passed to the ThreadPool
 * during construction. These functions are called once for each Worker at the beginning
 * and at the end of its lifetime.
 *
 * A ThreadPool can be running or stopped (#isRunning, #start, #stop). The ThreadPool is
 * automatically stopped in the destructor if it was running before (and will block and
 * wait for all remaining tasks to be finished. If this behavior is not desired, the
 * ThreadPool should be #stop%ped manually before destruction.
 */
class ThreadPool {
public:
    BooleanType(RunRemainingTasks);
    BooleanType(DetachThreads);

    /**
     * Constructor that initializes and starts \p nThreads Worker objects.
     *
     * \param nThreads The number of parallel threads of execution managed by the
     *        ThreadPool
     * \param workerInit The additional initialize function for each Worker that gets
     *        called once for each Worker when it is created
     * \param workerDeinit The additional deinitialize function for each Worker that gets
     *        called once for each Worken when it is destroyed
     * \param tpc The ghoul::thread::ThreadPriorityClass of the worker threads managed by
     *        the ThreadPool
     * \param tpl The ghoul::thread::ThreadPriorityLevel of the worker threads managed by
     *        the ThreadPool
     * \param bg Whether the worker threads managed by this thread pool are run in a
     *        background mode (depending on the support of the operating system)
     *
     * \pre \p nThreads must be bigger than 0
     * \pre \p workerInit must not be empty
     * \pre \p workerDeinit must not be empty
     * \post The ThreadPool is running
     */
    ThreadPool(int nThreads = 1, std::function<void()> workerInit = [](){},
        std::function<void()> workerDeinit = [](){},
        thread::ThreadPriorityClass tpc = thread::ThreadPriorityClass::Normal,
        thread::ThreadPriorityLevel tpl = thread::ThreadPriorityLevel::Normal,
        thread::Background bg = thread::Background::No);

    /**
     * Destructor that will block and wait for all remaining Tasks to be finished if the
     * ThreadPool is still running by calling the #stop method. If the ThreadPool has been
     * stopped, it will not call this function and thus not block.
     */
    ~ThreadPool();

    /**
     * Starts the previously #stop%ped ThreadPool and activates its Worker%s.
     *
     * \pre The ThreadPool must not be running
     * \post The ThreadPool is running
     */
    void start();

    /**
     * Stops the previously running ThreadPool. Depending on the passed parameters, this
     * function might block until all remaining tasks have been processed.
     *
     * \param runTasks If RunRemainingTasks::Yes, all remaining tasks in the queue will be
     *        finished; if RunRemainingTasks::No, they will be discarded
     * \param detachThreads If DetachThreads::Yes, the worker threads will be detached and
     *        finish their job independently from the current thread and, thus, this
     *        function will return immediately. if DetachThreads::No, this function will
     *        block until all workers have finished. Potential exceptions that occur
     *        during detaching or joining will be caught and an error message will be
     *        logged
     *
     * \pre The ThreadPool must be running
     * \pre Cannot run remaining tasks and detach threads
     *      `!(runTasks == Yes && detachThreads == Yes)`
     * \post The ThreadPool is stopped
     */
    void stop(RunRemainingTasks runTasks = RunRemainingTasks::Yes,
        DetachThreads detachThreads = DetachThreads::No
    );

    /**
     * Returns `true` if the ThreadPool is running, `false` otherwise.
     *
     * \return `true` if the ThreadPool is running, `false` otherwise
     */
    bool isRunning() const;

    /**
     * Resizes the ThreadPool such that the number of workers in the pool is \p nThreads
     * after this function call. If \p nThreads is bigger than the current number of
     * workers, additional workers are created and initialized, if \p nThreads is smaller
     * the extra workers detach and finish their work before being terminated. This
     * function can be called whether the ThreadPool is running or stopped.
     *
     * \param nThreads The new number of worker threads in this ThreadPool
     *
     * \pre nThreads must be bigger than 0
     * \post The ThreadPool contains \p nThreads workers
     */
    void resize(int nThreads);

    /**
     * Returns the number of workers that are managed by this ThreadPool.
     *
     * \return The number of workers that are managed by this ThreadPool
     */
    int size() const;

    /**
     * Returns the number of currently idle workers in this ThreadPool.
     *
     * \return The number of currently idle workers in this ThreadPool
     */
    int idleThreads() const;

    /**
     * Returns the number of remaining tasks waiting to be processed by this ThreadPool.
     *
     * \return The number of remaining tasks waiting to be processed by this ThreadPool
     */
    int remainingTasks() const;

    /**
     * Removes the remaining tasks from the waiting list, discarding them.
     *
     * \post The number of remaining tasks is empty
     */
    void clearRemainingTasks();

    /**
     * This function queues a task and returns an `std::future` object that holds a
     * potential return value of the function. The common use-case is passing a lambda
     * expression to this function that either returns a value or just performs its task
     * on the referenced values. All tasks passed to this functions are potentially
     * executed in parallel unless this ThreadPool was initialized with only a single
     * worker in the constructor or a subsequent call to #resize. The template parameters
     * of this function are best to be automatically determined. Example use-case:
     * ```
     * ghoul::ThreadPool pool(2);
     *
     * {
     *     std::future<int> ret = pool.queue([](){ return 1337; });
     *     auto urn = pool.queue([](){ return "foobar"; });
     *     ghoul_assert(ret.get() == 1337);
     *     ghoul_assert(urn.get() == "foobar");
     * }
     *
     * {
     *     auto func = [](int i, float f, std::string s) {
     *         return std::tuple(s, f, i);
     *     };
     *
     *     std::future<std::tuple<std::string, float, int>> ret =
     *         pool.queue(func, 1, 2.f, "3");
     *     std::tuple<std::string, float, int> val = ret.get();
     *     ghoul_assert("3" == std::get<0>(val));
     *     ghoul_assert(2.f == std::get<1>(val));
     *     ghoul_assert(1 == std::get<2>(val));
     * }
     * ```
     *
     * \tparam Function The description of the \p function%'s signature that will be
     *         called
     * \tparam Args A variable list of arguments that can be passed to the \p function
     * \param function The function that will be called. This can be any callable object,
     *        such as `std::function`, a `lamdba` expression, a `struct` with overloaded
     *        `operator()` or others
     * \param arguments The potential list of arguments passed to the \p function
     * \return A future containing the result of the evaluation of \p function with the
     *         passed \p arguments. If the function does not return anything, an
     *         `std::future<void>` is returned
     */
    template <typename Function, typename... Args>
    auto queue(Function&& function, Args&&... arguments)
        -> std::future<decltype(function(arguments...))>;

    /**
     * This function queues a `std::packaged_task` and returns its `std::future` object
     * that holds a potential return value. All tasks passed to this functions are
     * potentially executed in parallel unless this ThreadPool was initialized with only a
     * single worker in the constructor or a subsequent call to #resize. The template
     * parameters of this function are best to be automatically determined.
     *
     * \tparam T The type information of the `std::packaged_task` that is to be executed
     * \tparam Args A variable list of arguments that can be passed to the \p task
     * \param task The task that will be executed
     * \param arguments The potential list of arguments passed to the \p task
     * \return A future containing the result of the evaluation of \p task with the passed
     *         \p arguments
     */
    template <typename T, typename... Args>
    auto queue(std::packaged_task<T>&& task, Args&&... arguments)
        -> decltype(task.get_future());

private:
    ThreadPool(const ThreadPool&) = delete;
    ThreadPool(ThreadPool&&) = delete;
    ThreadPool& operator=(const ThreadPool&) = delete;
    ThreadPool& operator=(ThreadPool&&) = delete;

    /// A single task that is executed. This is a functional wrapper around the function +
    /// arguments that are passed in the queue method so that we can store all tasks in a
    /// single list
    using Task = std::function<void()>;

    /**
     * A worker object that consists of a thread and a boolean flag that determines
     * whether the worker should terminatate (or rather return out of the infinite loop).
     */
    struct Worker {
        /// The thread that grabs a task from the ThreadPool or waits until there is a
        /// task. This is stored as a unique_ptr in order to make the storage in a vector
        /// easier
        std::unique_ptr<std::thread> thread;
        /// If this is 'true', the thread will return, and thus end, instead of working on
        /// a new task. This is stored as a shared_pointer as this value is used in the
        /// ThreadPool as well as the lambda expression that drives the thread
        std::shared_ptr<std::atomic<bool>> shouldTerminate;
    };

    /**
     * This class represents a thin wrapper around `std::queue` that provides `std::mutex`
     * protection for the available methods, thus making them thread-safe to use. As soon
     * as there is a better adapter pattern for the STL classes that works in a concurrent
     * environment, this class is not needed anymore.
     */
    class TaskQueue {
    public:
        /**
         * Returns the top element of the queue and whether this item existed. If the
         * queue was empty, `{ Task(), false }` is returned, otherwise the second argument
         * to the `tuple` is `true`.
         *
         * \return A tuple containing either the top element of the queue and `true`, or a
         *         default constructed Task and `false`
         */
        std::tuple<Task, bool> pop();

        /**
         * Pushes the \p task to the bottom of the `std::queue`.
         *
         * \param task The task to be pushed onto the queue
         */
        void push(const Task& task);

        /**
         * Returns whether the queue is empty.
         *
         * \return `true` if the queue is empty
         */
        bool isEmpty() const;

        /**
         * Returns the size of the queue.
         *
         * \return The size of the queue
         */
        int size() const;

    private:
        /// The queue of tasks
        std::queue<ThreadPool::Task> _queue;

        /// The mutex protecting the queue. As the mutex is also required by const
        /// functions, it is declared 'mutable'
        mutable std::mutex _queueMutex;
    };

    /**
     * Activate the \p worker by creating a `std::thread` with the lambda expression that
     * will do all of the work inside the Worker. This function will overwrite the values
     * of the passed \p worker.
     *
     * \param worker The worker to be set by this function
     */
    void activateWorker(Worker& worker);

    /// The list of all workers managed by this ThreadPool
    std::vector<Worker> _workers;

    /// The list of remaining tasks that might be addressed by the available Worker%s
    std::shared_ptr<TaskQueue> _taskQueue;

    /// `true` if the ThreadPool is currently running, false otherwise
    std::shared_ptr<std::atomic_bool> _isRunning;

    /// The number of Worker%s that are currently waiting for a task
    std::shared_ptr<std::atomic_int> _nWaiting;

    /// This mutex guards pushing to the queue
    std::mutex _queueMutex;

    /// The mutex used by the `condition_variable` `_cv` used to wait for and wake up
    /// Worker%s based on incoming Task%s
    std::shared_ptr<std::mutex> _mutex;

    /// The condition variable that is used to wake up Worker%s when new Task%s are
    /// incoming. Used in combination with `_mutex`
    std::shared_ptr<std::condition_variable> _cv;

    /// The user-defined function that is called at initialization for each of the Worker
    /// threads
    std::function<void ()> _workerInitialization;

    /// The user-defined function that is called at deinitialization at the end of each
    /// Worker%s lifetime
    std::function<void ()> _workerDeinitialization;

    /// The ghoul::thread::ThreadPriorityClass of all the Worker%s of this ThreadPool
    thread::ThreadPriorityClass _threadPriorityClass;
    /// The ghoul::thread::ThreadPriorityLevel of all the Worker%s of this ThreadPool
    thread::ThreadPriorityLevel _threadPriorityLevel;
    /// Whether all Worker%s of this ThreadPool are started in the background mode (if
    /// supported by the operating system)
    thread::Background _threadBackground;
};

} // namespace ghoul

#include "threadpool.inl"

#endif // __GHOUL___THREADPOOL___H__
