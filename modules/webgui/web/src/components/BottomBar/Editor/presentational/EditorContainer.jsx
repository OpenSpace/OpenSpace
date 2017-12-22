import React from 'react'
import PropTypes from 'prop-types'
import Draggable from 'react-draggable'
import styles from '../style/EditorCanvas.scss'
import Histogram from '../containers/Histogram'
import Envelope from '../containers/Envelope'

const EditorContainer = ({
  height,
  width,
  activeVolume
}) => (
  <div >
    <div className={styles.EnvelopeContainer}>
          <Envelope
            height={height}
            width={width}
            activeVolume={activeVolume}
          />
    </div>
    <div className={styles.HistogramContainer}>
      <Histogram height={height} width={width} activeVolume={activeVolume}/>
    </div>
  </div>
)

export default EditorContainer


