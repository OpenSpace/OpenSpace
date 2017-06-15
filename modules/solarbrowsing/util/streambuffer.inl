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

namespace openspace {

template<typename T>
StreamBuffer<T>::StreamBuffer()
    : _concurrentJobManager(std::make_shared<globebrowsing::ThreadPool>(4))
{}

template<typename T>
void StreamBuffer<T>::removeJob(std::shared_ptr<StreamJob<T>> job) {
    _enqueuedJobIds.erase(job->id());
}

template<typename T>
bool StreamBuffer<T>::hasJob(const std::string& id) {
    return _enqueuedJobIds.count(id);
}

template<typename T>
int StreamBuffer<T>::numJobs() {
    return _enqueuedJobIds.size();
}

template<typename T>
void StreamBuffer<T>::enqueueJob(std::shared_ptr<StreamJob<T>> job) {
    _queue.push(job->id());
    _enqueuedJobIds.insert(job->id());
    //_enqueuedJobs[job->id()] = job;
    _concurrentJobManager.enqueueJob(job);
}

template<typename T>
std::shared_ptr<T> StreamBuffer<T>::popFinishedJob() {
    while (_concurrentJobManager.numFinishedJobs() > 0) {
        std::shared_ptr<globebrowsing::Job<T>> job = _concurrentJobManager.popFinishedJob();
        std::shared_ptr<StreamJob<T>> streamJob = std::dynamic_pointer_cast<StreamJob<T>>(job);

        // If id is not enqueued simply remove it
        const std::string& id = streamJob->id();
        if (_enqueuedJobIds.find(streamJob->id()) == _enqueuedJobIds.end()){
            continue;
        }

        // Add popped job to finished jobs
        _finishedJobs[streamJob->id()] = streamJob;

        // Look at front if id queue
        std::string queueFrontId = _queue.front();
        // // If finished job has front queue id, pop it and return
        if (_finishedJobs.count(queueFrontId) > 0) {
            auto finishedJob = _finishedJobs[queueFrontId];
            _queue.pop();
            _enqueuedJobIds.erase(queueFrontId);
            _finishedJobs.erase(queueFrontId);
            return finishedJob->product();
        }
        //return job->product();
    }
    return nullptr;
}

template<typename T>
void StreamBuffer<T>::clear() {
    while (_concurrentJobManager.numFinishedJobs() > 0) {
        _concurrentJobManager.popFinishedJob();
    }

    _concurrentJobManager.clearEnqueuedJobs();
    _enqueuedJobIds.clear();
    while (!_queue.empty()) {
        _queue.pop();
    }
    _finishedJobs.clear();
}

} // namespace openspace
