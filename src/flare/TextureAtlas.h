/*
 * Author: Victor Sand
 *
 */

#ifndef TEXTUREATLAS_H_
#define TEXTUREATLAS_H_

#define real float

namespace osp {

class Texture3D;

class TextureAtlas {
public:
  static TextureAtlas * New();
  ~TextureAtlas();

  bool Init();
  void SetBrickDimensions(unsigned int _xBrickDim,
                          unsigned int _yBrickDim,
                          unsigned int _zBrickDim);
  void SetNumBricks(unsigned int _xNumBricks,
                    unsigned int _yNumBricks,
                    unsigned int _zNumBricks);

  // Update a chosen brick with new data 
  // Assuming the brick data is ordered inorder for an individual brick
  bool UpdateBrick(unsigned int _brickIndex, real *_brickData);

  Texture3D * TexturePtr() { return texture_; }
  
private:
  TextureAtlas();
  TextureAtlas(const TextureAtlas&);
  unsigned int xBrickDim_;
  unsigned int yBrickDim_;
  unsigned int zBrickDim_;
  unsigned int xNumBricks_;
  unsigned int yNumBricks_;
  unsigned int zNumBricks_;
  Texture3D * texture_;
};

}

#endif
