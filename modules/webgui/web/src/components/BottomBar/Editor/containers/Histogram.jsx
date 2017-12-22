import React, {Component} from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import DataManager from '../../../../api/DataManager';
import { HistogramKey, UnitKey, MinKey, MaxKey } from '../../../../api/keys';
import HistogramCanvas from '../presentational/HistogramCanvas';

const normalizeHistogramDataToCanvas = (data, width, height) => {
  var normalizedHistogramData;
  var convertedData = (eval('('+data.Value+')'));
  if(data.length < width) {
    let maxValue =  Math.max.apply(Math,data.map(function(o){return o;}));
    normalizedHistogramData = data.map((value, index) =>
        Object.assign({},
            {x: (index / data.length) * width,
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
    state.transferfunctions.map(transferfunction => {
      if(transferfunction.id === ownProps.activeVolume) {
        histogram = normalizeHistogramDataToCanvas(transferfunction.data.Histogram.value, ownProps.width, ownProps.height);
        minValue = transferfunction.data.MinValue.value;
        maxValue = transferfunction.data.MaxValue.value;
        unit = transferfunction.data.DataUnit.value;
      }
    })
    return {
      histogram,
      minValue,
      maxValue,
      unit
    }
};

const Histogram = connect(
  mapStateToProps,
  )(HistogramCanvas)
export default Histogram;
