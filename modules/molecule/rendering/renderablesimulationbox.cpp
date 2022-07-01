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

    constexpr openspace::properties::Property::PropertyInfo SimulationBoxInfo = {
        "SimulationBox",
        "Simulation Box",
        "Size of the periodic simulation box (x/y/z)"
    };

    constexpr openspace::properties::Property::PropertyInfo AnimationSpeedInfo = {
        "AnimationSpeed",
        "Animation Speed",
        "Adjust the speed of the animation (seconds/s)"
    };

    constexpr openspace::properties::Property::PropertyInfo CollisionRadiusInfo = {
        "CollisionRadius",
        "Collision Radius",
        "Radius of the collision sphere around molecules"
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

        // [[codegen::verbatim(SimulationBoxInfo.description)]]
        glm::dvec3 simulationBox;

        // [[codegen::verbatim(AnimationSpeedInfo.description)]]
        std::optional<float> animationSpeed;

        // [[codegen::verbatim(CollisionRadiusInfo.description)]]
        float collisionRadius;
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
  _angularVelocity(AngularVelocityInfo),
  _simulationBox(SimulationBoxInfo),
  _animationSpeed(AnimationSpeedInfo),
  _collisionRadius(CollisionRadiusInfo)
{
  
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _moleculeFile = p.moleculeFile;
    _trajectoryFile = p.trajectoryFile.value_or("");
    _moleculeCount = p.moleculeCount;
    _linearVelocity = p.linearVelocity;
    _angularVelocity = p.angularVelocity;
    _simulationBox = p.simulationBox;
    _animationSpeed = p.animationSpeed.value_or(1.f);
    _collisionRadius = p.collisionRadius;
  
    // addProperty(_moleculeFile);
    // addProperty(_trajectoryFile);
    // addProperty(_moleculeCount);
    addProperty(_linearVelocity);
    addProperty(_angularVelocity);
    addProperty(_simulationBox);
    addProperty(_animationSpeed);
  
  // add the molecules
  
  ghoul::Dictionary d;
  d.setValue<std::string>("MoleculeFile", _moleculeFile);
  d.setValue<std::string>("TrajectoryFile", _trajectoryFile);
  auto renderable = std::make_shared<RenderableMolecule>(d);
  
  for (int i = 0; i < _moleculeCount; i++) {
    molecule_t demoMolecule {
      renderable,
      linearRand(dvec3(0.0), _simulationBox.value()),  // position
      0.0,                                             // angle
      sphericalRand(_linearVelocity.value()),          // direction
      sphericalRand(_angularVelocity.value()),         // rotation
    };
    _molecules.push_back(std::move(demoMolecule));
  }
  
  _renderables.push_back(renderable);
}

void RenderableSimulationBox::initialize() {
  for (auto& renderable: _renderables) {
    renderable->initialize();
  }
}

void RenderableSimulationBox::initializeGL() {
  for (auto& renderable: _renderables) {
    renderable->initializeGL();
  }
}

void RenderableSimulationBox::deinitializeGL() {
  for (auto& molecule: _molecules) {
    molecule.renderable->deinitializeGL();
  }
}

bool RenderableSimulationBox::isReady() const {
  bool isReady = true;
  for (const auto& renderable: _renderables) {
    isReady &= renderable->isReady();
  }
  return isReady;
}

void RenderableSimulationBox::render(const RenderData& data, RendererTasks& tasks) {
  for (auto& molecule: _molecules) {
    RenderData copyData = data;
    copyData.modelTransform.translation -= _simulationBox.value() * 0.5; // center the box
    copyData.modelTransform.translation += molecule.position;
    copyData.modelTransform.rotation = dmat4(copyData.modelTransform.rotation) * rotate(dmat4(1.0), molecule.angle, molecule.rotation);
    molecule.renderable->render(copyData, tasks);
  }
}

void RenderableSimulationBox::update(const UpdateData& data) {
  double dt = data.time.j2000Seconds() - data.previousFrameTime.j2000Seconds();
  dt *= _animationSpeed;

  // TODO: just doing some shit here, I need to implement proper brownian later
  // std::vector<dvec3> samples(1000);
  
  // update positions / rotations
  for (auto& molecule: _molecules) {
    // for (auto& sample : samples) {
    //   sample = ballRand(_linearVelocity.value());
    // }

    // molecule.direction = std::reduce(samples.begin(), samples.end(), -molecule.direction, [&](dvec3 a, dvec3 b) {
    //   auto d1 = dot(a, molecule.direction);
    //   auto d2 = dot(a, molecule.direction);
    //   return d1 > d2 ? a : b;
    // });

    molecule.position += molecule.direction * dt;
    molecule.position = mod(molecule.position, _simulationBox.value());
    molecule.angle += length(molecule.rotation) * dt;
  }

  // compute collisions
  for (auto it1 = _molecules.begin(); it1 != _molecules.end(); ++it1) {
    for (auto it2 = std::next(it1); it2 != _molecules.end(); ++it2) {
      
      molecule_t& m1 = *it1;
      molecule_t& m2 = *it2;
      
      double dist = distance(m1.position, m2.position);
      double intersection = 2.0 * _collisionRadius - dist;

      if (dist && intersection > 0.0) { // collision detected
        // swap the direction components normal to the collision plane from the 2
        // molecules. (simplistic elastic collision of 2 spheres with same mass)
        dvec3 dir = (m2.position - m1.position) / dist;
        dvec3 compM1 = dir * dot(m1.direction, dir);
        dvec3 compM2 = -dir * dot(m2.direction, -dir);
        m1.direction = m1.direction - compM1 + compM2;
        m2.direction = m2.direction - compM2 + compM1;

        // move the spheres away from each other (not intersecting)
        m1.position += -dir * intersection;
        m2.position += dir * intersection;
      }
      
    }
  }

  for (auto& renderable: _renderables) {
    renderable->update(data);
  }
}

