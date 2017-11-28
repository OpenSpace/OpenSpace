import React, {Component} from 'react';
import styles from './GraphBody.scss';
import PropTypes from 'prop-types';
class GraphBody extends Component {

  constructor(props) {
    super(props);
  }

  prepareData() {
    let d = [`M ${this.props.points[0].x} ${this.props.y - this.props.points[0].y}`];
    let collector = this.props.points.map((point, index) => {
      if (index != 0) {
        let xNext = point.x;
        let yNext = this.props.y - point.y ;
        return `L ${xNext} ${yNext}`;
      }
    });
    return d.concat(collector).join(' ');
  }

  renderWithLinearGradient(data) {
    return(
      <g>
        <linearGradient id="linear-gradient" gradientUnits="userSpaceOnUse" x1="0" y1={this.props.height} x2={this.props.width } y2={this.props.height} >
          {this.props.points.map((point, index) =>
            <stop key={index} offset={point.x / this.props.width} stopColor={point.color} stopOpacity="0%"/>
            )}
        </linearGradient>
      <path d={data}
        style={{stroke:'url(#linear-gradient)'}}
        strokeWidth={this.props.strokeWidth}
        fill={this.props.fill}
        fillOpacity={this.props.fillOpacity}/>
      </g>
    )
  }

  renderNormalGraph(data) {
    return(
      <path d={data}
        stroke={this.props.color}
        strokeWidth={this.props.strokeWidth}
        fill={this.props.fill}
        fillOpacity={this.props.fillOpacity}/>
    )
  }

  render() {
    let d = this.prepareData();
    if (this.props.UseLinearGradient) {
      return this.renderWithLinearGradient(d);
    }
    else{
      return this.renderNormalGraph(d);
    }
  }
}
GraphBody.propTypes = {
  UseLinearGradient: PropTypes.bool.isRequired
}
export default GraphBody;
