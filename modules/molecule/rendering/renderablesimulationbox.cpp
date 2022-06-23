#include "renderablesimulationbox.h"
#include "openspace/documentation/documentation.h"
#include "openspace/util/updatestructures.h"

#include <glm/gtc/random.hpp>
#include <numeric>

using namespace openspace;
using namespace glm;

namespace {
    constexpr const char* _loggerCat = "RenderableSimulationBox";

    constexpr openspace::properties::Property::PropertyInfo MoleculeFileInfo = {
        "MoleculeFile",
        "Molecule File",
        "Molecule file path"
    };

    constexpr openspace::properties::Property::PropertyInfo TrajectoryFileInfo = {
        "TrajectoryFile",
        "Trajectory File",
        "Trajectory file path"
    };

    constexpr openspace::properties::Property::PropertyInfo MoleculeCountInfo = {
        "MoleculeCount",
        "Molecule Count",
        "Count of molecules to simulate"
    };

    constexpr openspace::properties::Property::PropertyInfo LinearVelocityInfo = {
        "LinearVelocity",
        "Linear Velocity",
        "Average linear velocity at the start of the simulation (m/s)"
    };

    constexpr openspace::properties::Property::PropertyInfo AngularVelocityInfo = {
        "AngularVelocity",
        "Angular Velocity",
        "Average angular velocity at the start of the simulation (radians/s)"
    };

    struct [[codegen::Dictionary(RenderableSimulationBox)]] Parameters {
        // [[codegen::verbatim(MoleculeFileInfo.description)]]
        std::string moleculeFile;

        // [[codegen::verbatim(TrajectoryFileInfo.description)]]
        std::optional<std::string> trajectoryFile;

        // [[codegen::verbatim(MoleculeCountInfo.description)]]
        int moleculeCount;

        // [[codegen::verbatim(LinearVelocityInfo.description)]]
        float linearVelocity;

        // [[codegen::verbatim(AngularVelocityInfo.description)]]
        float angularVelocity;
    };

#include "renderablesimulationbox_codegen.cpp"
}

documentation::Documentation RenderableSimulationBox::Documentation() {
  return codegen::doc<Parameters>("molecule_renderablesimulationbox");
}

RenderableSimulationBox::RenderableSimulationBox(const ghoul::Dictionary& dictionary):
  Renderable(dictionary),
  _moleculeFile(MoleculeFileInfo),
  _trajectoryFile(TrajectoryFileInfo),
  _moleculeCount(MoleculeCountInfo),
  _linearVelocity(LinearVelocityInfo),
  _angularVelocity(AngularVelocityInfo)
{
  
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _moleculeFile = p.moleculeFile;
    _trajectoryFile = p.trajectoryFile.value_or("");
    _moleculeCount = p.moleculeCount;
    _linearVelocity = p.linearVelocity;
    _angularVelocity = p.angularVelocity;
  
    // addProperty(_moleculeFile);
    // addProperty(_trajectoryFile);
    // addProperty(_moleculeCount);
    addProperty(_linearVelocity);
    addProperty(_angularVelocity);
  
  // add the molecules
  
  ghoul::Dictionary d;
  d.setValue<std::string>("MoleculeFile", _moleculeFile);
  d.setValue<std::string>("TrajectoryFile", _trajectoryFile);
  
  for (int i = 0; i < _moleculeCount; i++) {
    molecule_t demoMolecule {
      std::make_unique<RenderableMolecule>(d),
      dvec3(0.0),                              // position
      0.0,                                     // angle
      sphericalRand(_linearVelocity.value()),  // direction
      sphericalRand(_angularVelocity.value()), // rotation
    };
    _molecules.push_back(std::move(demoMolecule));
  }
}

void RenderableSimulationBox::initialize() {
  for (auto& molecule : _molecules) {
    molecule.renderable->initialize();
  }
}

void RenderableSimulationBox::initializeGL() {
  for (auto& molecule : _molecules) {
    molecule.renderable->initializeGL();
  }
}

void RenderableSimulationBox::deinitializeGL() {
  for (auto& molecule : _molecules) {
    molecule.renderable->deinitializeGL();
  }
}

bool RenderableSimulationBox::isReady() const {
  bool isReady = true;
  for (const auto& molecule : _molecules) {
    isReady &= molecule.renderable->isReady();
  }
  return isReady;
}

void RenderableSimulationBox::render(const RenderData& data, RendererTasks& tasks) {
  for (auto& molecule : _molecules) {
    RenderData copyData = data;
    copyData.modelTransform.translation += molecule.position;
    copyData.modelTransform.rotation = dmat4(copyData.modelTransform.rotation) * rotate(dmat4(1.0), molecule.angle, molecule.rotation);
    molecule.renderable->render(copyData, tasks);
  }
}

void RenderableSimulationBox::update(const UpdateData& data) {
  double dt = data.time.j2000Seconds() - data.previousFrameTime.j2000Seconds();
  std::cout << dt << std::endl;

  // TODO: just doing some shit here, I need to implement proper brownian later
  std::vector<dvec3> samples(1000);

  for (auto& molecule : _molecules) {
    for (auto& sample : samples) {
      sample = ballRand(_linearVelocity.value());
    }

    molecule.direction = std::reduce(samples.begin(), samples.end(), -molecule.direction, [&](dvec3 a, dvec3 b) {
      auto d1 = dot(a, molecule.direction);
      auto d2 = dot(a, molecule.direction);
      return d1 > d2 ? a : b;
    });

    molecule.position += molecule.direction * dt;
    molecule.angle += length(molecule.rotation) * dt;
  }

  for (auto& molecule : _molecules) {
    molecule.renderable->update(data);
  }
}

