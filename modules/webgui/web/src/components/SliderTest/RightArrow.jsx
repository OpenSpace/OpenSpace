import React, { Component } from 'react';
import Icon from '../common/Icon/Icon';

// Functional component
const RightArrow = (props) => {
  return (
    <div onClick={props.nextSlide} className="right-arrow">
      <Icon icon="keyboard_arrow_right"/>
    </div>
  );
}

export default RightArrow;