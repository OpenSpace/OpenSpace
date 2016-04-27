/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __CONCURRENT_JOB_MANAGER_H__
#define __CONCURRENT_JOB_MANAGER_H__

#include <glm/glm.hpp>
#include <memory>
#include <ostream>
#include <thread>
#include <queue>
#include <atomic>

#include <modules/globebrowsing/other/concurrentqueue.h>

#include <ghoul/misc/assert.h>



namespace openspace {


    // Templated abstract base class representing a job to be done.
    // Client code derive from this class and implement the virtual execute() method
    template<typename P>
    struct Job {

        Job() { }
        virtual ~Job() { }

        virtual void execute() = 0;
        virtual P product() = 0;
        
    };




    /* 
     * Templated Concurrent Job Manager
     * This class is used execute specific jobs on one (1) parallell thread
     */
    template<typename P>
    class ConcurrentJobManager{
    public:
        ConcurrentJobManager()
        : _hasWorkingThread(false)
        {

        }

        ~ConcurrentJobManager() {

        }


        void enqueueJob(std::shared_ptr<Job<P>> job) {
            _incomingJobs.push(job);
            if (!_hasWorkingThread) {
                _hasWorkingThread = true; // Can only be set to true by the main thread
                executeJobsInSeparateThread();
            }
        }

        std::shared_ptr<Job<P>> popFinishedJob() {
            ghoul_assert(_finishedJobs.size() > 0, "There is no finished job to pop!");
            return _finishedJobs.pop();
        }

        size_t numFinishedJobs() {
            return _finishedJobs.size();
        }


    
    private:


        void executeJobsInSeparateThread() {
            // Create new thread and run workerThreadMainTask on that thread
            std::thread t(&ConcurrentJobManager::workerThreadMainTask, this);
            t.detach();
        }
        
        void workerThreadMainTask() {
            while (_incomingJobs.size() > 0) {
                auto job = _incomingJobs.pop();                

                job->execute();

                _finishedJobs.push(job);
            }

            _hasWorkingThread = false; // Can only be set to false by worker thread
        }

        ConcurrentQueue<std::shared_ptr<Job<P>>> _incomingJobs;
        ConcurrentQueue<std::shared_ptr<Job<P>>> _finishedJobs;

        // Using this atomic bool is probably not optimal - Should probably
        // use a conditional variable instead
        std::atomic<bool> _hasWorkingThread;

    };


} // namespace openspace



#endif // __CONCURRENT_JOB_MANAGER_H__
