import React from 'react';
import styles from '../style/Envelope.scss';
import GraphBody from '../../../common/Graph/GraphBody';
import PointPosition from './PointPosition';
import Point from './Point';

const pointsForEnvelopeGraph = (data, height) => {
  const convertedData = [];
  data.forEach((point) => {
    const tmpObject = Object.assign({},
      { x: point.position.x,
        y: height - point.position.y,
        color: point.color },
    );
    convertedData.push(tmpObject);
  });
  return convertedData;
};

const EnvelopeCanvas = ({
  handleClick,
  handleDrag,
  height,
  width,
  envelopes,
  pointPositions,
  minValue,
  maxValue,
}) => (
  <div>
    {(envelopes.length !== 0) && (
      <div>
        <PointPosition
          className={styles.Line}
          points={pointPositions}
          width={width}
          height={height}
          envelopes={envelopes}
          minValue={minValue}
          maxValue={maxValue}
        />
        {envelopes.map(envelope =>
          (<div key={envelope.id}>
            <svg className={styles.Line} height={height} width={width}>
              <GraphBody
                UseLinearGradient
                points={pointsForEnvelopeGraph(envelope.points, height)}
                x={0}
                y={600}
                width={width}
                fillOpacity={'0'}
                strokeWidth={2}
              />
            </svg>
            {envelope.points.map((point, index) =>
              (<Point
                className={styles.Envelope}
                key={point.id}
                handleClick={() => handleClick(envelope, point.id)}
                handleDrag={(e, ui) => handleDrag(e, ui, index, envelope)}
                height={height}
                width={width}
                {...point}
                bounds={(index === 0) ? { x1: 0, x2: envelope.points[1].position.x } :
                  (index === envelope.points.length - 1) ? { x1: envelope.points[envelope.points.length - 2].position.x, x2: width } :
                    { x1: envelope.points[0].position.x, x2: envelope.points[envelope.points.length - 1].position.x }}
                active={!!((point.active || envelope.active))}
              />),
            )}
          </div>),
        )}
      </div>
    )}
  </div>
);
/* EnvelopeCanvas.propTypes = {
  HandleDrag: PropTypes.func.isRequired,
  handleClick: PropTypes.func.isRequired,
  points: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y: PropTypes.number.isRequired,
            }).isRequired,
      anchor: PropTypes.bool.isRequired,
      color: PropTypes.string.isRequired,
    }).isRequired,
  ).isRequired,
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  active: PropTypes.bool.isRequired,
} */
export default EnvelopeCanvas;
