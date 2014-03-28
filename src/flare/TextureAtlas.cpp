/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */

#include <flare/TextureAtlas.h>
#include <flare/Utils.h>
#include <vector>

using namespace osp;

TextureAtlas::TextureAtlas()
  : xBrickDim_(0), yBrickDim_(0), zBrickDim_(0),
    xNumBricks_(0), yNumBricks_(0), zNumBricks_(0), 
    texture_(NULL) {
}

TextureAtlas * TextureAtlas::New() {
  return new TextureAtlas;
}

TextureAtlas::~TextureAtlas() {
  if (texture_ != NULL) {
    delete texture_;
  }
}

void TextureAtlas::SetBrickDimensions(unsigned int _xBrickDim,
                                      unsigned int _yBrickDim,
                                      unsigned int _zBrickDim) {
  xBrickDim_ = _xBrickDim;
  yBrickDim_ = _yBrickDim;
  zBrickDim_ = _zBrickDim;
}

void TextureAtlas::SetNumBricks(unsigned int _xNumBricks,
                                unsigned int _yNumBricks,
                                unsigned int _zNumBricks) {
  xNumBricks_ = _xNumBricks;
  yNumBricks_ = _yNumBricks;
  zNumBricks_ = _zNumBricks;
}

bool TextureAtlas::Init() {

  INFO("Initializing TextureAtlas");

  if (xBrickDim_ == 0 || yBrickDim_ == 0 || zBrickDim_ == 0) {
    ERROR("TextureAtlas::Init() One or more dimensions are zero");
    return false;
  }

  if (xNumBricks_ == 0 || yNumBricks_ == 0 || zNumBricks_ == 0) {
    ERROR("TextureAtlas::Init() One or more number of bricks are zero");
    return false;
  }

  unsigned int numBricksTotal = xNumBricks_*yNumBricks_*zNumBricks_;
 
  std::vector<unsigned int> dims;
  dims.push_back(xBrickDim_ * numBricksTotal);
  dims.push_back(yBrickDim_);
  dims.push_back(zBrickDim_);
  
  texture_ = new ghoul::opengl::Texture(glm::size3_t(xBrickDim_ * numBricksTotal,yBrickDim_,zBrickDim_), ghoul::opengl::Texture::Format::RGBA, GL_RGBA, GL_FLOAT);
  texture_->uploadTexture();
  //texture_ = Texture3D::New(dims);
  //if (!texture_->Init()) return false;

  return true;

}

bool TextureAtlas::UpdateBrick(unsigned int _brickIndex, real *_brickData) {
  
  // Bricks should be laid out along the x axis
  unsigned int xOffset =_brickIndex * xBrickDim_;
  unsigned int yOffset = 0;
  unsigned int zOffset = 0;
  unsigned int xSize = xBrickDim_;
  unsigned int ySize = yBrickDim_;
  unsigned int zSize = zBrickDim_;
  glm::size3_t dim = texture_->dimensions();
    glGetError();
    glBindTexture(GL_TEXTURE_3D, *texture_);
    glTexSubImage3D(GL_TEXTURE_3D,
                    0,
                    xOffset,
                    yOffset,
                    zOffset,
                    xSize,
                    ySize,
                    zSize,
                    GL_RED,
                    GL_FLOAT,
                    NULL);
    glBindTexture(GL_TEXTURE_3D, 0);
    
    return (CheckGLError("Texture3D::UpdateSubRegion") == GL_NO_ERROR);
/*
  if (!texture_->UpdateSubRegion(xOffset, yOffset, zOffset,
                                 xSize, ySize, zSize, _brickData)) {
    return false;
  }

  return true;
  */
}


