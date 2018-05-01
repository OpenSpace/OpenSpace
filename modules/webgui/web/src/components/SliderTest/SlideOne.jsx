import React, { Component } from 'react';
import imageOne from './story1.png';

// Functional component
const SlideOne= (props) => {

    let background = {
        backgroundImage: imageOne,
        backgroundSize: 'cover',
        backgroundPosition: 'center'
    }
  
    return (
    <div style={background} className="slide">
        <img src={imageOne} alt={'Story 1'} />
    </div>
    );
}

export default SlideOne;