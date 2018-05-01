import React, { Component } from 'react';
import imageTwo from './story2.png';

// Functional component
const SlideTwo= (props) => {

    let background = {
        backgroundImage: imageTwo,
        backgroundSize: 'cover',
        backgroundPosition: 'center'
    }
  
    return (
    <div style={background} className="slide">
        <img src={imageTwo} alt={'Story 2'} />
    </div>
    );
}

export default SlideTwo;