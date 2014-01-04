#ifndef OBJECT_H
#define OBJECT_H

namespace openspace {

#define sharedDataInstance_ sgct::SharedData::instance()

class Object {
public:
	Object();

	virtual void serialize() const;
	virtual void deserialize() const;

	virtual void encode();
	virtual void decode();
};
	
} // namespace openspace

#endif