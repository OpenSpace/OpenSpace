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





namespace openspace {


    // Templated abstract base class representing a job to be done.
    // Client code derive from this class and implement the virtual execute() method
    template<typename P>
    struct Job {

        Job() { }
        virtual ~Job() { }

        virtual void execute() = 0;
        virtual P product() = 0;
        
        /*
        enum Status {
            UNKNOWN,
            QUEUED,
            IN_PROGRESS,
            FINISHED
        };

        Status status;
        */
        
    };




    // Templated 
    template<typename P>
    class ConcurrentJobManager{
    public:
        ConcurrentJobManager() {

        }

        ~ConcurrentJobManager() {

        }



        void enqueueFutureJob(std::unique_ptr<Job<P>> job) {
            dummy = std::move(job);
        }

        void startInSeparateThread() {
            // create new thread
            dummy->execute();
        }

        std::unique_ptr<Job<P>> popFinishedJob() {
            return std::move(dummy);
        }


    
    private:
        
        void workerThreadMainTask() {
            
        }

        std::unique_ptr<Job<P>> dummy;

    };


} // namespace openspace


#include <modules/globebrowsing/other/concurrentjobmanager.inl>

#endif // __CONCURRENT_JOB_MANAGER_H__
