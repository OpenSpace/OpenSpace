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
    var envelopes;
    state.sceneGraph.map(element => {
          if(element.name === "Enlil Sequence") {
            envelopes = element.subowners[1].subowners[1].properties[1].Value;
            histogram = normalizeHistogramDataToCanvas(element.subowners[1].subowners[1].properties[2].Value, ownProps.width, ownProps.height);
            minValue = element.subowners[1].subowners[1].properties[3].Value;
            maxValue = element.subowners[1].subowners[1].properties[4].Value;
            unit = element.subowners[1].subowners[1].properties[5].Value;
          }
        })
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
