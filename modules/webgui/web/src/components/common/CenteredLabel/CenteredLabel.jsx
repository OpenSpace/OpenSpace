import React from 'react';
import PropTypes from 'prop-types';

import { centeredLabel } from './CenteredLabel.scss';

const CenteredLabel = props => (
  <div {...props} className={`${props.className} ${centeredLabel}`}>
    { props.children }
  </div>
);

CenteredLabel.propTypes = {
  children: PropTypes.node.isRequired,
  className: PropTypes.string,
};

CenteredLabel.defaultProps = {
  className: '',
};

export default CenteredLabel;
