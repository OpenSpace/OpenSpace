import React, {Component} from 'react';
import styles from './GraphBody.scss';
class GraphBody extends Component {

  prepareData() {
    let d = [`M ${this.props.x} ${this.props.y}`];

    let collector = this.props.data.map(chunk => {
      let xNext = chunk[0] * 160;
      let yNext = this.props.height - chunk[1] * 100;
      return `L ${xNext} ${yNext}`;
    });

    return d.concat(collector).join(' ');
  }

  render() {
    let d = this.prepareData();
    return(
      <path d={d} className={styles.Graph}
        stroke={this.props.color}
        strokeWidth={1}
        fill="below"/>
    )
  }
}

export default GraphBody;
