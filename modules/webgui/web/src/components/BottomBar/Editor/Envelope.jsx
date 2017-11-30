import React, {Component} from 'react'
import PropTypes from 'prop-types';
import Draggable from 'react-draggable'
import Point from './containers/Point'
import styles from './Envelope.scss'
import GraphBody from '../../common/Graph/GraphBody'
class Envelope extends Component {
  constructor(props) {
    super(props);

    this.handleOnDrag = this.props.handleOnDrag.bind(this);
  }

  pointsForEnvelopeGraph(data) {
    return data = this.props.points.map(point =>
        Object.assign({},
        {x: point.position.x + 10,
         y: 600 - point.position.y - 10,
         color: point.color}
        )
      )
  }

  render() {
    const {id, points, color, anchor, active, height, width, envelope} = this.props;
    return (
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
        <div key={point.id}>
        <Point
          {...point}
          height={height}
          width={width}
          envelope={envelope}
          handleOnDrag={(position, id, envelopeId) => this.handleOnDrag(position, id, envelopeId)}
         />
        </div>
        )}
      </div>
    );
  }
}
Envelope.propTypes = {
  handleOnDrag: PropTypes.func.isRequired,
  points: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
      anchor: PropTypes.bool.isRequired,
      color: PropTypes.string.isRequired,
    }).isRequired,
  ).isRequired,
}
export default Envelope;
