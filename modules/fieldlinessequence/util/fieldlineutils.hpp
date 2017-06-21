#include <ghoul/logging/logmanager.h>

namespace openspace {

enum class BatsrusTopology {
    SolarWind = 0,
    North = 1,
    South = 2,
    Closed = 3
};

enum class PfssTopology {
    Closed = 0,
    OpenInward = 1,
    OpenOutward = 2
};

//!-- Returns -1 if error occured!
int getFieldlineTopology(const FieldlinesState::Model& model,
                         const glm::vec3& endPoint1,
                         const glm::vec3& endPoint2,
                         const float& radius) {

    float f1 = glm::length(endPoint1);
    float f2 = glm::length(endPoint2);
    bool np, sp;
    np = f1 < radius ? true : false;
    sp = f2 < radius ? true : false;
    if (model == FieldlinesState::Model::batsrus) {

        if      ( np &&  sp)  { return static_cast<int>(BatsrusTopology::Closed); }
        else if (!np && !sp)  { return static_cast<int>(BatsrusTopology::SolarWind); }
        else if ( np && !sp)  { return static_cast<int>(BatsrusTopology::North); }

        return static_cast<int>(BatsrusTopology::South);

    } else if (model == FieldlinesState::Model::pfss) {

        LERRORC("FL-Utils","PFSS model topology is not yet supported!");
        // TODO: Not supported yet!
        // if      ( np &&  sp)  { return static_cast<int>(PfssTopology::Closed); }
        // else if (!np &&  sp)  { return static_cast<int>(PfssTopology::OpenInward); }
        // else if ( np && !sp)  { return static_cast<int>(PfssTopology::OpenOutward); }

        return -1;

    } else if (model == FieldlinesState::Model::enlil) {
        LERRORC("FL-Utils","Enlil don't have different topologies!");
    } else {
        // Unrecognized model
        LERRORC("FL-Utils","Model is not recognized!");
        return -1;
    }
}

} // namespace openspace
