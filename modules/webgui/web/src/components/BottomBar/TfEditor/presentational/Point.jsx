import React from 'react';
import PropTypes from 'prop-types';
import DraggableCore from 'react-draggable';
import styles from '../style/Point.scss';

const Point = ({
  handleClick,
  handleDrag,
  height,
  width,
  color,
  active,
  anchor,
  bounds,
  position,
}) => (
  <div>
    <DraggableCore
      defaultPosition={{ x: position.x - 10, y: position.y - 10 }}
      onDrag={handleDrag}
      axis={anchor ? 'x' : 'both'}
      bounds={{ top: -10, left: bounds.x1 - 10, right: bounds.x2 - 10, bottom: height - 10 }}
    >
      <svg
        className={active ? styles.Active : styles.Point}
        width={20}
        height={20}
        onDrag={handleDrag}
        onClick={handleClick}
      >
        <circle cx={10} cy={10} r={active ? 8 : 10} fill={color} />
      </svg>
    </DraggableCore>
  </div>
);
Point.propTypes = {
  handleDrag: PropTypes.func.isRequired,
  handleClick: PropTypes.func.isRequired,
  position: PropTypes.shape({
    x: PropTypes.number.isRequired,
    y: PropTypes.number.isRequired,
  }).isRequired,
  anchor: PropTypes.bool.isRequired,
  color: PropTypes.string.isRequired,
  active: PropTypes.bool.isRequired,
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
};

export default Point;
