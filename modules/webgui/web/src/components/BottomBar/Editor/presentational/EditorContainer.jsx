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
  <div >
    <div className={styles.EnvelopeContainer}>
        {envelopes.map(envelope =>
          <Envelope
            key={envelope.id}
            {...envelope}
            height={height}
            width={width}
          />
        )}
    </div>
    <div className={styles.HistogramContainer}>
      <Histogram height={height} width={width}/>
    </div>
  </div>
)

export default EditorContainer


