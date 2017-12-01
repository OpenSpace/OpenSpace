import React from 'react'
import PropTypes from 'prop-types'

const EnvelopeCanvas = ({
  handleClick,
  handleDrag,
  height,
  width,
  color,
  active,
  anchor,
  position
}) => (
    <div className={styles.Envelope}>
      <svg className={styles.Line} height={height} width={width + 10}>
        <GraphBody
         UseLinearGradient={true}
         points={this.pointsForEnvelopeGraph(points)}
         x={0}
         y={600}
         width={800}
         fillOpacity={"0"}
         strokeWidth={2}
        />
      </svg>
     {points.map((point) =>
      <Point
      key={point.id}
        handleClick={() => this.handleClick(point.id)}
        handleDrag={(e, ui) => this.handleDrag(e, ui, point.id)}
        height={height}
        width={width}
        {...point}
        active={(point.active || active) ? true : false}

      />
      )}
    </div>
)
EnvelopeCanvas.propTypes = {
  handleDrag: PropTypes.func.isRequired,
  handleClick: PropTypes.func.isRequired,
  position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
  anchor: PropTypes.bool.isRequired,
  color: PropTypes.string.isRequired,
  active: PropTypes.bool.isRequired,
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
}

export default EnvelopeCanvas
