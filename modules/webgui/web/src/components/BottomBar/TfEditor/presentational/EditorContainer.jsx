import React from 'react';
import styles from '../style/EditorCanvas.scss';
import Histogram from '../containers/Histogram';
import Envelope from '../containers/Envelope';

const EditorContainer = ({
  height,
  width,
  activeVolume,
  URI,
}) => (
  <div className={styles.EditorContainer}>
    <div className={styles.EnvelopeContainer}>
      <Envelope
        height={height}
        width={width}
        activeVolume={activeVolume}
        URI={URI}
      />
    </div>
    <div className={styles.HistogramContainer}>
      <Histogram height={height} width={width} activeVolume={activeVolume} />
    </div>
  </div>
);

export default EditorContainer;
