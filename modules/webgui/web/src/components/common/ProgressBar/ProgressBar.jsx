import React from 'react';
import PropTypes from 'prop-types'; 
import styles from './ProgressBar.scss';

import Label from '../Label/Label';

const epsilon = 0.001;

const ProgressBar = (props) => {

    return (
        <div className={styles.container}>
            <Label>{props.label + (props.progressPercent >= (100.0 - epsilon) ? ' (complete)' : '')}</Label>
            <div className={styles.bar}>
                <div style={{width:`${props.progressPercent}%`, 
                    backgroundColor: props.colorFill, 
                    height: '19px'}}/>
            </div>
        </div>
    )
};

ProgressBar.propTypes = {
  progressPercent: PropTypes.number,
  colorFill: PropTypes.string,
};

ProgressBar.defaultProps = {
  progressPercent: 0,
  colorFill: '#aff7c0',
}

export default ProgressBar;
