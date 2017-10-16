import React, {Component} from 'react'
import PropTypes from 'prop-types';
import Draggable from 'react-draggable'
import Point from './Point'
import styles from './Envelope.scss'
import GraphBody from '../../common/Graph/GraphBody'
const Envelope = ({
  id,
  points,
  color,
  anchor,
  active,
  handleOnDrag,
  handleOnClick,
}) => (
    <div>
      <svg className={styles.Line}>
        <GraphBody
         points={points}
         color={color}
         x={0}
         y={600}
         fillOpacity={"0"}
         strokeWidth={2}
       />
      </svg>
      <div className={styles.Envelope}>
       {points.map((point) =>
        <Point
          key={point.id}
          {...point}
          color={color}
          active={active}
          handleOnDrag={(position, id) => handleOnDrag(position, id)}
          handleOnClick={() => handleOnClick()}
         />
        )}
      </div>
    </div>
  );
Envelope.propTypes = {
  handleOnDrag: PropTypes.func.isRequired,
  handleOnClick: PropTypes.func.isRequired,
  points: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
      anchor: PropTypes.bool.isRequired,
    }).isRequired,
  ).isRequired,
  color: PropTypes.string.isRequired,
  active: PropTypes.bool.isRequired,
}
export default Envelope;
