import React, {Component} from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import DataManager from '../../../../api/DataManager';
import { HistogramKey, UnitKey, MinKey, MaxKey } from '../../../../api/keys';
import HistogramCanvas from '../presentational/HistogramCanvas';

class Histogram extends Component {
  constructor(props) {
    super(props);
    this.state = {
      histogramLoaded: false,
      unit: "",
      minValue: 0,
      maxValue: 0,
      NormalizedHistogramData: [],
    }
    this.handleRecievedHistogram = this.handleRecievedHistogram.bind(this);
    this.changeUnit = this.changeUnit.bind(this);
    this.changeMinValue = this.changeMinValue.bind(this);
    this.changeMaxValue = this.changeMaxValue.bind(this);
  }

  componentDidMount() {
    DataManager.subscribe(HistogramKey, this.handleRecievedHistogram);
    DataManager.subscribe(UnitKey, this.changeUnit);
    DataManager.getValue(MinKey, this.changeMinValue);
    DataManager.getValue(MaxKey, this.changeMaxValue);
  }

  handleRecievedHistogram(data) {
    var convertedData = (eval('('+data.Value+')'));
    return this.normalizeHistogramDataToCanvas(convertedData);
  }

  changeUnit(data) {
    this.setState({unit: data.Value});
  }

  changeMaxValue(data) {
    this.setState({maxValue: Number(data.Value)});
  }

  changeMinValue(data) {
    this.setState({minValue: Number(data.Value)});
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
      this.state.NormalizedHistogramData.push({ x: this.props.width, y: 0 });
      this.setState({ histogramLoaded : true})
    }
  }

  render() {
    const {histogramLoaded, NormalizedHistogramData, unit, minValue, maxValue } = this.state;
    const { height, width, color } = this.props;
      return (
      <div>
        {histogramLoaded && (
            <HistogramCanvas data={NormalizedHistogramData} height={height} width={width} color={color} unit={unit} minValue={minValue} maxValue={maxValue} />
        )}
      </div>
    );
    }
};
export default Histogram;
