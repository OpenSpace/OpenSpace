import React, {Component} from 'react';
import Axis from './Axis';
import GraphBody from './GraphBody';

class Graph extends Component {

  render() {
    return (
      <div>
      <svg width={this.props.width} height={this.props.height}>
        <GraphBody
          {...this.props}
          x={0}
          y={this.props.height}
        />
      </svg>
      </div>
    )
  }
}

export default Graph
