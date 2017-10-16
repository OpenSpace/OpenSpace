import React, {Component} from 'react';
import styles from './GraphBody.scss';
class GraphBody extends Component {

  prepareData() {
    let d = [`M ${this.props.points[0].position.x} ${this.props.points[0].position.y}`];
    let collector = this.props.points.map(point => {
      let xNext = point.position.x;
      let yNext = point.position.y;
      return `L ${xNext} ${yNext}`;
    });
    return d.concat(collector).join(' ');
  }

  render() {
    let d = this.prepareData();
    return(
      <path d={d}
        stroke={this.props.color}
        strokeWidth={this.props.strokeWidth}
        fill={this.props.fill}
        fillOpacity={this.props.fillOpacity}/>
    )
  }
}

export default GraphBody;
