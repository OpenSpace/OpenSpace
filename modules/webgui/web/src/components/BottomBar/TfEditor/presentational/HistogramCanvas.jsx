import React from 'react';
import PropTypes from 'prop-types';
import GraphBody from '../../../common/Graph/GraphBody';


const HistogramCanvas = ({
  histogram,
  height,
  width,
  unit,
  minValue,
  maxValue,
}) => (
  <div>
    <svg width={width} height={height}>
      <GraphBody
        UseLinearGradient={false}
        color={'blue'}
        points={histogram}
        x={0}
        y={600}
        fill={'blue'}
        fillOpacity={'.5'}
        strokeWidth={1}
      />
      <text x={width - 40} y={height - 10} fontFamily={'Verdana'} fontSize={10} fill={'white'}>
        {unit}
      </text>
    </svg>
  </div>
);
HistogramCanvas.PropTypes = {
  histogram: PropTypes.arrayOf(
    PropTypes.shape({
      x: PropTypes.number.isRequired,
      y: PropTypes.number.isRequired,
    }),
  ).isRequired,
};
export default HistogramCanvas;
