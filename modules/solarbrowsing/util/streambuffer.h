/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___STREAMBUFFER___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___STREAMBUFFER___H__

#include <modules/globebrowsing/other/concurrentjobmanager.h>
#include <modules/globebrowsing/other/threadpool.h>

#include <iostream>

namespace openspace {

/**
 * Extend job with an id so that an old job can never be processed when buffer has
 * been cleared. Override execute and product functions in sub class.
 */
template<typename T>
class StreamJob : public globebrowsing::Job<T> {
public:
    StreamJob(const std::string& id) : _id(id) {}
    //virtual ~StreamJob() override;
    virtual void execute() override = 0;
    virtual std::shared_ptr<T> product() override = 0;

    const std::string& id() { return _id; }
private:
    std::string _id;
};

/**
 * The streambuffer is a queue for streaming frames or other
 * data that is meant to be processed in order - only 1 thread can be used using the
 * concurrent job manager for the jobs to be processed in order.
 * It keeps track of unique id's to never pop an already enqueud job
 * when cleared.
 */
template<typename T>
class StreamBuffer {
public:
    StreamBuffer();

    int numJobs();
    void removeJob(std::shared_ptr<StreamJob<T>> job);
    void enqueueJob(std::shared_ptr<StreamJob<T>> job);

    std::shared_ptr<T> popFinishedJob();
    void clear();
    globebrowsing::ConcurrentJobManager<T> _concurrentJobManager;
private:
    std::unordered_multiset<std::string> _enqueuedJobIds;
};

} // namespace openspace

#include "streambuffer.inl"

#endif // __OPENSPACE_MODULE_BASE___STREAMBUFFER___H__
