import React, {Component} from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import DataManager from '../../../../api/DataManager';
import HistogramCanvas from '../presentational/HistogramCanvas';

const normalizeHistogramDataToCanvas = (Value, width, height) => {
  var normalizedHistogramData;
  var convertedData = (eval('('+Value+')'));
  if(convertedData.length < width) {
    let maxValue =  Math.max.apply(Math,convertedData.map(function(o){return o;}));
    normalizedHistogramData = convertedData.map((value, index) =>
        Object.assign({},
            {x: (index / convertedData.length) * width,
             y: (value / maxValue) * height,
            },
      )
    )
    //Making sure the graphs body gets filled properly by adding 0 in the beginning and end of the histogram
    normalizedHistogramData.unshift({ x: 0, y: 0 });
    normalizedHistogramData.push({ x: width, y: 0 });
    return normalizedHistogramData;
  }
}

const mapStateToProps = (state, ownProps) => {
  var histogram, minValue, maxValue, unit;

  histogram = normalizeHistogramDataToCanvas(ownProps.activeVolume.properties.find(function (obj) 
    { return obj.id === "Histogram"; }).Value, ownProps.width, ownProps.height);

  unit = ownProps.activeVolume.properties.find(function (obj) { return obj.id === "DataUnit"; }).Value;
  minValue = Number(ownProps.activeVolume.properties.find(function (obj) { return obj.id === "MinValue"; }).Value);
  maxValue = Number(ownProps.activeVolume.properties.find(function (obj) { return obj.id === "MaxValue"; }).Value);
    return {
      histogram, 
      minValue,
      maxValue,
      unit,
    };
};

const Histogram = connect(
  mapStateToProps,
  )(HistogramCanvas)
export default Histogram;
