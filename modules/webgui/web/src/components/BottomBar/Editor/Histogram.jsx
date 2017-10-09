import React, {Component} from 'react';
import Graph from '../../common/Graph/Graph';
class Histogram extends Component {
  render() {
    return (
        <Graph {...this.props}/>
      );
  }
}
export default Histogram;
