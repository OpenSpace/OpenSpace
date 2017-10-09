import React from 'react';

export default class Axis extends React.Component {
  prepareCords() {
    let coords = {
      x1: this.props.x,
      y1: this.props.y
    }

    if(this.props.horizontal) {
      coords.x2 = coords.x1 + this.props.length;
      coords.y2 = coords.y1;
    } else {
      coords.x2 = coords.x1;
      coords.y2 = coords.y1 + this.props.length;
    }

    return coords;
  }

  render() {
    let coords = this.prepareCords();
    return (
      <line {...coords} stroke="green" strokeWidth={2} />
    )
  }
}
