import React from 'react'
import Dot from './Dot'

const DotsGroup = ({ index, images, dotClick}) => {
  // Map dots to images
  const dots = images.map((image, i) => {
    // Check if image is active or not
    let active = (i === index) ? true : false;
    return (
      <Dot
        key={i}
        id={i}
        active={active}
        dotClick={dotClick}
      />
    )
  })
  // Return the group of dots
  return (
    <div className="dots-container">
      { dots }
    </div>
  )
}

export default DotsGroup;