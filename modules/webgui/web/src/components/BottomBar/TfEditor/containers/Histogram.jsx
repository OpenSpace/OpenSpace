import React from 'react';
import { connect } from 'react-redux';
import HistogramCanvas from '../presentational/HistogramCanvas';

const normalizeHistogramDataToCanvas = (Value, width, height) => {
  let normalizedHistogramData;
  const convertedData = (eval(`(${Value})`));
  if (convertedData.length < width) {
    const maxValue = Math.max(...convertedData.map(o => o));
    normalizedHistogramData = convertedData.map((value, index) =>
      Object.assign({},
        { x: (index / convertedData.length) * width,
          y: (value / maxValue) * height,
        },
      ),
    );
    // Making sure the graphs body gets filled properly by adding 0 in the
    // beginning and end of the histogram
    normalizedHistogramData.unshift({ x: 0, y: 0 });
    normalizedHistogramData.push({ x: width, y: 0 });
    return normalizedHistogramData;
  }
};

const mapStateToProps = (state, ownProps) => {
  const histogram = normalizeHistogramDataToCanvas(ownProps.activeVolume.properties.find(obj => obj.id === 'Histogram').Value, ownProps.width, ownProps.height);

  const unit = ownProps.activeVolume.properties.find(obj => obj.id === 'DataUnit').Value;
  const minValue = Number(ownProps.activeVolume.properties.find(obj => obj.id === 'MinValue').Value);
  const maxValue = Number(ownProps.activeVolume.properties.find(obj => obj.id === 'MaxValue').Value);

  return {
    histogram,
    minValue,
    maxValue,
    unit,
  };
};

const Histogram = connect(
  mapStateToProps,
)(HistogramCanvas);
export default Histogram;
