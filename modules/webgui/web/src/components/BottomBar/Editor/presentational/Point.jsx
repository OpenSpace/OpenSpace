import React from 'react'
import PropTypes from 'prop-types'
import Draggable from 'react-draggable'
import styles from '../style/Point.scss';

const WTF = ({
  handleClick,
  handleDrag,
  height,
  width,
  color,
  active,
  anchor,
  position
}) => (
  <div>
  <Draggable defaultPosition={position} onDrag={handleDrag} axis={anchor ? "x" : "both"} bounds={{top: 0, left: 0, right: width, bottom: (height - 10)}}>
    <svg className={active ? styles.Active : styles.Point} width={20} height={20} onClick={handleClick}>
      <circle cx={10} cy={10} r={active ? 8 : 10} fill={color} />
    </svg>
  </Draggable>
  </div>
)
WTF.propTypes = {
  handleDrag: PropTypes.func.isRequired,
  position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
  anchor: PropTypes.bool.isRequired,
  color: PropTypes.string.isRequired,
  active: PropTypes.bool.isRequired,
}

export default WTF
