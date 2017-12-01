import React from 'react'
import PropTypes from 'prop-types'
import Draggable from 'react-draggable'
import styles from '../style/Point.scss';

const EditorCanvas = ({
  handleClick,
  handleDrag,
  height,
  width,
  color,
  active,
  anchor,
  position
}) => (
  <div className={styles.EditorContainer} >
    <button onClick={() => this.props.AddEnvelope(defaultEnvelopePoints)}>Add Envelope</button>
    <button onClick={() => this.props.DeleteEnvelope()}>Delete Envelope</button>
    <div className={styles.EnvelopeContainer}>
        {this.props.envelopes.map(envelope =>
          <Envelope className={styles.Envelope}
            key={envelope.id}
            onPointDrag={this.props.OnPointDrag}
            height={height}
            width={width}
            points={envelope.points}
            id={envelope.id}
            active={envelope.active}
          />
        )}
    </div>
    {histogramLoaded && (
    <div className={styles.HistogramContainer}>
      <Histogram className={styles.HistogramContainer} data={NormalizedHistogramData} height={height} width={width}/>
    </div>
  )}
  </div>
)
EditorCanvas.propTypes = {
  handleDrag: PropTypes.func.isRequired,
  position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
  anchor: PropTypes.bool.isRequired,
  color: PropTypes.string.isRequired,
  active: PropTypes.bool.isRequired,
}

export default EditorCanvas


