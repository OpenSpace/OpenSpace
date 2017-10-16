import React from 'react';
import Graph from '../../common/Graph/Graph';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import styles from './Histogram.scss'

const Histogram = ({
  histograms,
}) => (
      <div className={styles.Histogram}>
      {histograms.map(histogram =>
        <Graph
          key={histogram.id}
          {...histogram}
        />
      )}
    </div>
);
Histogram.PropTypes = {
  histograms: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      points: PropTypes.arrayOf(
        PropTypes.shape({
            id: PropTypes.number.isRequired,
            position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
          }).isRequired,
      ).isRequired,
      color: PropTypes.string.isRequired,
    }).isRequired
  ).isRequired,
}
const mapStateToProps = (state) => {
  return {
    histograms:
      state.histograms,
    };
};

const CreatedHistograms = connect(
  mapStateToProps,
)(Histogram);

export default CreatedHistograms;
