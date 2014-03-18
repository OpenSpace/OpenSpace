/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 * Construct a traversable TSP structure with structure and error info
 * from a .tsp file
 *
 */

#ifndef TSP_H_
#define TSP_H_

// Make sure to use 64 bits for file offset
#define _FILE_OFFSET_BITS 64

// For easy switching between offset types
// (off_t, off64_t etc)
#define off off64_t

#include <vector>
#include <list>
#include <string>
#include <iostream>

namespace osp {

class Config;
class TransferFunction;

class TSP {
public:

  enum NodeData {
    BRICK_INDEX = 0,
    CHILD_INDEX,
    SPATIAL_ERR,
    TEMPORAL_ERR,
    NUM_DATA
  };

  static TSP * New(Config * _config);
  ~TSP();

  // Struct for convenience
  // Note: UNUSED in this implementation
  struct Color {
    Color() {
      r = g = b = a = 0.f;
    }
    Color(float _r, float _g, float _b, float _a) {
      r = _r;
      g = _g;
      b = _b;
      a = _a;
    }
    float r;
    float g;
    float b;
    float a;
  };

  // Tries to read cached file
  bool ReadCache();
  // Write structure to cache
  bool WriteCache();

  bool ReadHeader();
  
  // Functions to build TSP tree and calculate errors
  bool Construct();
  bool CalculateSpatialError();
  bool CalculateTemporalError();

  int * Data() { return &data_[0]; }
  unsigned int Size() { return data_.size(); }

  // TODO support dimensions of differens sizes
  unsigned int BrickDim() const { return xBrickDim_; }
  unsigned int PaddedBrickDim() const { return paddedBrickDim_; }
  unsigned int NumBricksPerAxis() const { return xNumBricks_; }
  unsigned int NumOrigTimesteps() const { return numOrigTimesteps_; }
  unsigned int NumTimesteps() const { return numTimesteps_; }
  unsigned int NumTotalNodes() const { return numTotalNodes_; }
  unsigned int NumValuesPerNode() const { return NUM_DATA; }
  unsigned int NumBSTNodes() const { return numBSTNodes_; }
  unsigned int NumOTNodes() const { return numOTNodes_; }
  unsigned int NumOTLevels() const { return numOTLevels_; }

private:
  TSP();
  TSP(Config *_config);
  TSP(const TSP&);
  
  Config *config_;

  // Holds the actual structure
  std::vector<int> data_;

  // Data from file
  unsigned int gridType_;
  unsigned int numOrigTimesteps_;
  unsigned int numTimesteps_;
  unsigned int xBrickDim_;
  unsigned int yBrickDim_;
  unsigned int zBrickDim_;
  unsigned int xNumBricks_;
  unsigned int yNumBricks_;
  unsigned int zNumBricks_;
  //unsigned int dataSize_;
  
  // Additional metadata
  unsigned int paddedBrickDim_;
  unsigned int numTotalNodes_;
  unsigned int numBSTLevels_;
  unsigned int numBSTNodes_;
  unsigned int numOTLevels_;
  unsigned int numOTNodes_;

  const unsigned int paddingWidth_ = 1;

  // Error stats
  float minSpatialError_;
  float maxSpatialError_;
  float medianSpatialError_;
  float minTemporalError_;
  float maxTemporalError_;
  float medianTemporalError_;

  // Returns a list of the octree leaf nodes that a given input 
  // brick covers. If the input is already a leaf, the list will
  // only contain that one index.
  std::list<unsigned int> CoveredLeafBricks(unsigned int _brickIndex);

  // Returns a list of the BST leaf nodes that a given input brick
  // covers (at the same spatial subdivision level).
  std::list<unsigned int> CoveredBSTLeafBricks(unsigned int _brickIndex);

  // Return a list of eight children brick incices given a brick index
  std::list<unsigned int> ChildBricks(unsigned int _brickIndex);

  //std::ios::pos_type dataPos_;
  // Position of first data entry (after header)
  off dataPos_;

  // Calculate weighted square distance between two RGBA colors
  // c2 should be an averaged or zero color
  float SquaredDist(Color _c1, Color _c2);


};

}

#endif

