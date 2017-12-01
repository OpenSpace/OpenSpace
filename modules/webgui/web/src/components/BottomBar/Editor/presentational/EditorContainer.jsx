import React from 'react'
import PropTypes from 'prop-types'
import Draggable from 'react-draggable'
import styles from '../style/EditorCanvas.scss'
import Histogram from '../containers/Histogram'
import Envelope from '../containers/Envelope'

const EditorContainer = ({
  height,
  width,
  envelopes
}) => (
  <div className={styles.EditorContainer} >
    <div className={styles.EnvelopeContainer}>
        {envelopes.map(envelope =>
          <Envelope className={styles.Envelope}
            key={envelope.id}
            {...envelope}
            height={height}
            width={width}
          />
        )}
    </div>
    <div className={styles.HistogramContainer}>
      <Histogram className={styles.HistogramContainer} height={height} width={width}/>
    </div>
  </div>
)

export default EditorContainer


