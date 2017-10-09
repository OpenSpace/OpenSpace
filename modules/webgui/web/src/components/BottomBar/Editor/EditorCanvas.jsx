import React, {Component} from 'react'
import Draggable from 'react-draggable'
import Histogram from './Histogram';
import PropTypes from 'prop-types';
import {createStore} from 'redux';

import styles from './EditorCanvas.scss'

class Circle extends Component {
  constructor(props) {
    super(props);
  }

  render() {
  return (
  <Draggable axis={this.props.axis}  defaultPosition={{x: this.props.x, y: this.props.y}} onDrag={() => this.props.handleDrag(this.props.position.x + 1)}>
    <circle r={10} fill={this.props.color} onClick={this.props.onClick}/>
  </Draggable>
  );
}
}

class Envelope extends Component {
  constructor(props) {
    super(props);

    this.state = {

        x:0,
        y:0,

    }

    }

    renderCircle(i) {
    return (
      <Circle
        {...this.props.circles[i]}
        onClick={() => this.props.onClick(i)}
        position={0}
        handleDrag={(position) => this.setState({position})}
      />
    );
  }

  render() {
    return (
      <svg className={styles.EditorCanvas}>
        {this.renderCircle(0)}
        {this.renderCircle(1)}
        {this.renderCircle(2)}
        {this.renderCircle(3)}
      </svg>
    );
  }
}

class EditorCanvas extends Component {

  constructor(props) {
    super(props);

    this.state = {
      circles: [
      {x: 100, y: this.props.height, color: "red", axis: "x"},
      {x: 150, y: 150, color: "blue" , axis: "both"},
      {x: 300, y: 150, color: "blue" , axis: "both"},
      {x: 300, y: this.props.height, color: "red" , axis: "x"},
      ],
    }
  }

  handleClick(i) {
    const circles = this.state.circles.slice();
    circles[i].x = this.state.lastX;
    circles[i].y = this.state.lastY;
  }

  handleDrag(test) {
    console.log('dragging' + this.state.test);
  }

  render() {
    return (
      <div className={styles.EditorContainer} >
        <Histogram className={styles.Histogram} {...this.props}/>
        <Envelope
          circles={this.state.circles}
          onClick={i => this.handleClick(i)}
        />
      </div>
    );
  }
};
export default EditorCanvas;
