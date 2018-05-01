import React, { Component } from 'react';
import Icon from '../common/Icon/Icon';

// Functional component
const LeftArrow = (props) => {
  return (
    <div onClick={props.previousSlide} className="left-arrow">
      <Icon icon="keyboard_arrow_left"/>
    </div>
  );
}

export default LeftArrow;