import React, {Component} from 'react';
import PropTypes from 'prop-types';
import DataManager from '../../../../api/DataManager';
import { HistogramKey } from '../../../../api/keys';
import HistogramCanvas from '../presentational/HistogramCanvas';

class Histogram extends Component {
  constructor(props) {
    super(props);
    this.state = {
      histogramLoaded: false,
      NormalizedHistogramData: [],
    }
    this.handleRecievedHistogram = this.handleRecievedHistogram.bind(this);
  }

  componentDidMount() {
    DataManager.subscribe(HistogramKey, this.handleRecievedHistogram);
  }

  handleRecievedHistogram(data) {
    var convertedData = (eval('('+data.Value+')'));
    return this.normalizeHistogramDataToCanvas(convertedData);
  }

  normalizeHistogramDataToCanvas(data) {
    if(data.length < this.props.width) {
      let maxValue =  Math.max.apply(Math,data.map(function(o){return o;}));
      this.state.NormalizedHistogramData = data.map((value, index) =>
          Object.assign({},
              {x: (index / data.length) * this.props.width,
               y: (value / maxValue) * this.props.height,
              },
        )
      )
      //Making sure the graphs body gets filled properly by adding 0 in the beginning and end of the histogram
      this.state.NormalizedHistogramData.unshift({ x: 0, y: 0 });
      this.state.NormalizedHistogramData.push({ x: this.state.width, y: 0 });
      this.setState({ histogramLoaded : true})
    }
  }

  render() {
    const {histogramLoaded, NormalizedHistogramData} = this.state;
    const { height, width, color } = this.props;
      return (
      <div >
      {histogramLoaded && (
          <HistogramCanvas data={NormalizedHistogramData} height={height} width={width} color={color}/>
      )}
      </div>
    );
    }
};
export default Histogram
