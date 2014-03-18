/* 
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */

#ifndef BRICKMANAGER_H
#define BRICKMANAGER_H

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <boost/timer/timer.hpp>
#include <stdio.h>

// Make sure we get 64 bits for offset
#define _FILE_OFFSET_BITS 64
// For easy switching between offset types
#define off off64_t

namespace osp {

class Texture3D;
class Config;

class BrickManager {
public:
  static BrickManager * New(Config *_config);
  ~BrickManager();

  Config *config_;

  enum BUFFER_INDEX { EVEN = 0, ODD };

  // Read header data from file, should normally only be called once
  // unless header data changes
  bool ReadHeader();

  bool InitAtlas();

  // Build brick list from request list
  // Resets values in _brickRequest to 0
  bool BuildBrickList(BUFFER_INDEX _bufIdx, std::vector<int> &_brickRequest);

  // Upload bricks from memory buffer to PBO using the brick list
  bool DiskToPBO(BUFFER_INDEX _pboIndex);

  // Init transfer from PBO to texture atlas
  bool PBOToAtlas(BUFFER_INDEX _pboIndex);

  std::vector<int> BrickList(BUFFER_INDEX _bufIdx) { 
    return brickLists_[_bufIdx]; 
  }

  Texture3D * TextureAtlas() { return textureAtlas_; }

  // Header accessors
  unsigned int GridType() const { return gridType_; }
  unsigned int NumOrigTimesteps() const { return numOrigTimesteps_; }
  unsigned int NumTimesteps() const { return numTimesteps_; }
  unsigned int XBrickDim() const { return xBrickDim_; }
  unsigned int YBrickDim() const { return yBrickDim_; }
  unsigned int ZBrickDim() const { return zBrickDim_; }
  unsigned int XNumBricks() const { return xNumBricks_; }
  unsigned int YNumBricks() const { return yNumBricks_; }
  unsigned int ZNumBricks() const { return zNumBricks_; }
  unsigned int PaddingWidth() const { return paddingWidth_; }
  unsigned int DataSize() const { return dataSize_; }

private:

  BrickManager();
  BrickManager(Config *_config);
  BrickManager(const BrickManager&);

  // Header data
  unsigned int gridType_;
  unsigned int numOrigTimesteps_;
  unsigned int numTimesteps_;
  unsigned int xBrickDim_;
  unsigned int yBrickDim_;
  unsigned int zBrickDim_;
  unsigned int xNumBricks_;
  unsigned int yNumBricks_;
  unsigned int zNumBricks_;
  unsigned int dataSize_;

  unsigned int numBricks_;
  unsigned int brickDim_;
  unsigned int paddedBrickDim_;
  unsigned int atlasDim_;

  const unsigned int paddingWidth_ = 1;

  unsigned int numBrickVals_;
  unsigned int numBricksFrame_;
  unsigned int numBricksTree_;
  unsigned int brickSize_;
  unsigned int volumeSize_;
  unsigned int numValsTot_;

  // Texture coordinates to be assigned
  int xCoord_;
  int yCoord_;
  int zCoord_;

  // Texture where the actual atlas is kept
  Texture3D *textureAtlas_;

  std::vector<std::vector<int> > brickLists_;

  // C-style I/O
  std::FILE *file_;
  off dataPos_;

  bool hasReadHeader_;
  bool atlasInitialized_;

  // PBOs
  unsigned int pboHandle_[2];

  // Caching, one for each PBO
  std::vector<std::vector<int> > bricksInPBO_;
  std::vector<std::vector<bool> > usedCoords_;

  // Increment the coordinate to be assigned (handle looping)
  void IncCoord();
  // Linear version of the current x, y, z coordinate to be assigned
  unsigned int LinearCoord(int _x, int _y, int _z);
  // 3D coordinates from linear index
  void CoordsFromLin(int _idx, int &_x, int &_y, int &_z); 

  // Fill a brick in the volume using a pointer to flattened brick data
  bool FillVolume(float *_in, 
                  float *_out,
                  unsigned int _x, 
                  unsigned int _y, 
                  unsigned int _z);

  // Timer and timer constants
  boost::timer::cpu_timer timer_;
  const double BYTES_PER_GB = 1073741824.0;

};

}

#endif
